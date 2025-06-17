pub mod builtin;
pub mod bytecode;
pub mod obj;
pub mod value;

use std::rc::Rc;

use kenspan::Span;

use crate::builtin::Builtin;
use crate::bytecode::{Chunk, Fetch, OpCode, OpStream};
use crate::obj::{Function, Obj};
use crate::value::{Value, try_le, try_lt};

#[derive(Debug, Clone)]
enum Status {
    Running,
    Finished(Value),
}

#[derive(Debug, Clone, Copy)]
struct Frame<'a> {
    stream: OpStream<'a>,
    bp:     usize,
}

impl<'a> Frame<'a> {
    #[must_use]
    const fn new(chunk: &'a Chunk, bp: usize) -> Self {
        Self {
            stream: OpStream::new(chunk),
            bp,
        }
    }

    #[inline]
    fn fetch<T>(&mut self) -> RuntimeResult<T>
    where
        OpStream<'a>: Fetch<T>,
    {
        Fetch::<T>::fetch(&mut self.stream).ok_or(RuntimeError::FetchError)
    }

    #[must_use]
    fn fetch_value(&self, at: usize) -> Option<Value> {
        self.stream.fetch_value(at)
    }

    const fn jump(&mut self, to: usize) {
        self.stream.jump(to);
    }

    #[must_use]
    const fn bp(&self) -> usize {
        self.bp
    }
}

pub struct Vm {
    stack:  Vec<Value>,
    local:  Vec<Value>,
    global: Vec<Value>,
}

impl Default for Vm {
    fn default() -> Self {
        let global = Builtin::core_builtins()
            .map(|b| Value::Obj(Rc::new(Obj::Builtin(b))))
            .collect();
        Self {
            global,
            stack: Vec::new(),
            local: Vec::new(),
        }
    }
}

impl Vm {
    #[must_use]
    const fn sp(&self) -> usize {
        self.stack.len()
    }

    #[must_use]
    const fn lp(&self) -> usize {
        self.local.len()
    }

    fn restore_local(&mut self, lp: usize) -> RuntimeResult<()> {
        if lp > self.lp() {
            return Err(RuntimeError::StackUnderflow);
        }
        self.local.truncate(lp);
        Ok(())
    }

    fn restore_stack(&mut self, sp: usize) -> RuntimeResult<()> {
        if sp > self.sp() {
            return Err(RuntimeError::StackUnderflow);
        }
        self.stack.truncate(sp);
        Ok(())
    }

    fn restore_all(&mut self, sp: usize, lp: usize) -> RuntimeResult<()> {
        self.restore_stack(sp)?;
        self.restore_local(lp)?;
        Ok(())
    }

    pub fn eval(&mut self, function: &Function) -> FrameResult<Value> {
        let (value, _) = self.run(function)?;
        Ok(value)
    }

    fn run<'a>(&mut self, function: &'a Function) -> FrameResult<(Value, Frame<'a>)> {
        let sp = self.sp();
        let lp = self.lp();

        let mut frame = self.create_frame(function);
        let ret = loop {
            match self.eval_next(&mut frame) {
                Ok(Status::Running) => {}
                Ok(Status::Finished(ret)) => break ret,
                Err(mut err) => {
                    let span = frame.fetch().unwrap_or_default();
                    let _ = self.restore_all(sp, lp);
                    err.spans.push(span);
                    return Err(err);
                }
            }
        };

        Ok((ret, frame))
    }

    fn create_frame<'a>(&self, function: &'a Function) -> Frame<'a> {
        let lp = self.lp();
        let bp = lp - usize::from(function.arity());
        Frame::new(function.chunk(), bp)
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn drain(&mut self, len: usize) -> RuntimeResult<Vec<Value>> {
        if self.sp() < len {
            return Err(RuntimeError::StackUnderflow);
        }
        let mut values = Vec::with_capacity(len);
        values.extend(self.stack.drain(self.stack.len() - len..));
        Ok(values)
    }

    fn prefix_op(
        &mut self,
        op: impl FnOnce(Value) -> RuntimeResult<Value>,
    ) -> RuntimeResult<Status> {
        let value = self.pop_stack()?;
        let value = op(value)?;
        self.push_stack(value);
        Ok(Status::Running)
    }

    #[inline]
    fn infix_op(
        &mut self,
        op: impl FnOnce(Value, Value) -> RuntimeResult<Value>,
    ) -> RuntimeResult<Status> {
        let rhs = self.pop_stack()?;
        let lhs = self.pop_stack()?;
        let value = op(lhs, rhs)?;
        self.push_stack(value);
        Ok(Status::Running)
    }

    #[inline]
    fn load_idx(&mut self) -> RuntimeResult<Status> {
        let idx = self.pop_stack()?;
        let indexed = self.pop_stack()?;
        match (indexed, idx) {
            (Value::MutObj(obj), Value::Int(idx)) => {
                let idx = usize::try_from(idx).map_err(|_| RuntimeError::OutOfBounds)?;
                let list = obj.as_list()?;
                let item = list.get(idx).ok_or(RuntimeError::OutOfBounds)?;
                self.push_stack(item.clone());

                Ok(Status::Running)
            }
            _ => Err(RuntimeError::TypeError),
        }
    }

    #[inline]
    fn store_idx(&mut self) -> RuntimeResult<Status> {
        let idx = self.pop_stack()?;
        let indexed = self.pop_stack()?;
        let value = self.pop_stack()?;
        match (indexed, idx) {
            (Value::MutObj(obj), Value::Int(idx)) => {
                let idx = usize::try_from(idx).map_err(|_| RuntimeError::OutOfBounds)?;
                let mut list = obj.as_list_mut()?;
                let item = list.get_mut(idx).ok_or(RuntimeError::OutOfBounds)?;
                *item = value;

                Ok(Status::Running)
            }
            _ => Err(RuntimeError::TypeError),
        }
    }

    #[inline]
    fn eval_next(&mut self, frame: &mut Frame<'_>) -> FrameResult<Status> {
        let op = frame.fetch()?;

        match op {
            OpCode::Nop => Ok(Status::Running),
            OpCode::Pop => {
                self.pop_stack()?;
                Ok(Status::Running)
            }

            OpCode::Push => {
                let at = frame.fetch()?;
                let value = frame.fetch_value(at).ok_or(RuntimeError::NoValue)?;
                self.push_stack(value);
                Ok(Status::Running)
            }

            OpCode::PushInt => {
                let int = frame.fetch::<u32>()?;
                let value = Value::Int(i64::from(int));
                self.push_stack(value);
                Ok(Status::Running)
            }

            OpCode::MakeList => {
                let len = frame.fetch()?;
                let values = self.drain(len)?;
                let list = Value::from(values);
                self.push_stack(list);
                Ok(Status::Running)
            }

            OpCode::MakeTuple => {
                let len = frame.fetch()?;
                let values = self.drain(len)?;
                let list = Value::from(values.into_boxed_slice());
                self.push_stack(list);
                Ok(Status::Running)
            }

            OpCode::Add => self.infix_op(|a, b| a + b).map_err(FrameError::from),
            OpCode::Sub => self.infix_op(|a, b| a - b).map_err(FrameError::from),
            OpCode::Mul => self.infix_op(|a, b| a * b).map_err(FrameError::from),
            OpCode::Div => self.infix_op(|a, b| a / b).map_err(FrameError::from),
            OpCode::Rem => self.infix_op(|a, b| a % b).map_err(FrameError::from),

            OpCode::Neg => self.prefix_op(|x| -x).map_err(FrameError::from),
            OpCode::Not => self.prefix_op(|x| !x).map_err(FrameError::from),

            OpCode::Eq => {
                let rhs = self.pop_stack()?;
                let lhs = self.pop_stack()?;
                let value = Value::Bool(lhs == rhs);
                self.push_stack(value);
                Ok(Status::Running)
            }

            OpCode::Lt => self.infix_op(try_lt).map_err(FrameError::from),
            OpCode::Le => self.infix_op(try_le).map_err(FrameError::from),

            OpCode::Call => {
                let count = frame.fetch::<u8>()?;
                let args = self.drain(count as usize)?;
                let value = self.pop_stack()?;

                match value.as_obj()?.as_ref() {
                    Obj::Function(function) => {
                        if function.arity() != count {
                            return Err(FrameError::from(RuntimeError::ArityError));
                        }
                        self.local.extend(args);
                        let (ret, frame) = self.run(function)?;
                        self.restore_local(frame.bp())?;
                        self.push_stack(ret);
                        Ok(Status::Running)
                    }
                    Obj::Builtin(builtin) => {
                        let value = builtin(&args)?;
                        self.push_stack(value);
                        Ok(Status::Running)
                    }
                    _ => Err(FrameError::from(RuntimeError::TypeError)),
                }
            }
            OpCode::AddLocal => {
                let value = self.pop_stack()?;
                self.local.push(value);
                Ok(Status::Running)
            }
            OpCode::AddGlobal => {
                let value = self.pop_stack()?;
                self.global.push(value);
                Ok(Status::Running)
            }
            OpCode::LoadIdx => self.load_idx().map_err(FrameError::from),
            OpCode::StoreIdx => self.store_idx().map_err(FrameError::from),
            OpCode::Load => {
                let at = frame.fetch::<usize>()?;
                let value = self
                    .local
                    .get(frame.bp() + at)
                    .ok_or(RuntimeError::LocalNotFound)?;
                self.push_stack(value.clone());
                Ok(Status::Running)
            }
            OpCode::Store => {
                let at = frame.fetch::<usize>()?;
                let value = self.pop_stack()?;
                let local = self
                    .local
                    .get_mut(frame.bp() + at)
                    .ok_or(RuntimeError::LocalNotFound)?;
                *local = value;
                Ok(Status::Running)
            }
            OpCode::Restore => {
                let count = frame.fetch::<usize>()?;
                let lp = self.lp() - count;
                self.restore_local(lp)?;
                Ok(Status::Running)
            }
            OpCode::LoadGlobal => {
                let at = frame.fetch::<usize>()?;
                let value = self.global.get(at).ok_or(RuntimeError::GlobalNotFound)?;
                self.push_stack(value.clone());
                Ok(Status::Running)
            }
            OpCode::StoreGlobal => {
                let at = frame.fetch::<usize>()?;
                let value = self.pop_stack()?;
                let global = self
                    .global
                    .get_mut(at)
                    .ok_or(RuntimeError::GlobalNotFound)?;
                *global = value;
                Ok(Status::Running)
            }
            OpCode::Jmp => {
                let ip = frame.fetch::<usize>()?;
                frame.jump(ip);
                Ok(Status::Running)
            }
            OpCode::JmpIfNot => {
                let ip = frame.fetch::<usize>()?;
                let cond: bool = self.pop_stack()?.try_into()?;
                if !cond {
                    frame.jump(ip);
                }
                Ok(Status::Running)
            }
            OpCode::Ret => {
                let ret = self.pop_stack()?;
                Ok(Status::Finished(ret))
            }
        }
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum RuntimeError {
    #[error("out of bounds access")]
    OutOfBounds,
    #[error("division by zero")]
    DivisionByZero,
    #[error("type error")]
    TypeError,
    #[error("arity error")]
    ArityError,
    #[error("not enought arguments")]
    NotEnoughArgs,
    #[error("borrow error")]
    BorrowError,
    #[error("stack underflow")]
    StackUnderflow,
    #[error("local not found")]
    LocalNotFound,
    #[error("global not found")]
    GlobalNotFound,
    #[error("fetch error")]
    FetchError,
    #[error("no value")]
    NoValue,
}

pub struct FrameError {
    runtime: RuntimeError,
    spans:   Vec<Span>,
}

impl FrameError {
    #[must_use]
    pub const fn runtime(&self) -> RuntimeError {
        self.runtime
    }

    #[must_use]
    pub fn spans(&self) -> &[Span] {
        &self.spans
    }
}

impl From<RuntimeError> for FrameError {
    fn from(value: RuntimeError) -> Self {
        Self {
            runtime: value,
            spans:   Vec::new(),
        }
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type FrameResult<T> = Result<T, FrameError>;
