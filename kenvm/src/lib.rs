pub mod builtin;
pub mod bytecode;
pub mod obj;
pub mod value;

use kenspan::Span;

use crate::builtin::Builtin;
use crate::bytecode::{Chunk, Op, OpStream};
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
        let global = Builtin::core_builtins().map(Value::Builtin).collect();
        Self {
            stack: Vec::new(),
            local: Vec::new(),
            global,
        }
    }
}

impl Vm {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

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
                    let span = frame.stream.fetch_span();
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

    fn grab_args(&mut self, count: u8) -> RuntimeResult<Vec<Value>> {
        let count = usize::from(count);
        if self.sp() < count {
            return Err(RuntimeError::StackUnderflow);
        }
        let mut args = Vec::with_capacity(count);
        args.extend(self.stack.drain(self.stack.len() - count..));
        Ok(args)
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
    fn eval_next(&mut self, frame: &mut Frame<'_>) -> FrameResult<Status> {
        let op = frame.stream.fetch().ok_or(RuntimeError::NoInstruction)?;

        match op {
            Op::Nop => Ok(Status::Running),
            Op::Pop => {
                self.pop_stack()?;
                Ok(Status::Running)
            }

            Op::Push(at) => {
                let value = frame.fetch_value(at).ok_or(RuntimeError::NoValue)?;
                self.push_stack(value);
                Ok(Status::Running)
            }

            Op::PushInt(int) => {
                let value = Value::Int(i64::from(int));
                self.push_stack(value);
                Ok(Status::Running)
            }

            Op::Add => self.infix_op(|a, b| a + b).map_err(FrameError::from),
            Op::Sub => self.infix_op(|a, b| a - b).map_err(FrameError::from),
            Op::Mul => self.infix_op(|a, b| a * b).map_err(FrameError::from),
            Op::Div => self.infix_op(|a, b| a / b).map_err(FrameError::from),

            Op::Neg => self.prefix_op(|x| -x).map_err(FrameError::from),
            Op::Not => self.prefix_op(|x| !x).map_err(FrameError::from),

            Op::Eq => {
                let rhs = self.pop_stack()?;
                let lhs = self.pop_stack()?;
                let value = Value::Bool(lhs == rhs);
                self.push_stack(value);
                Ok(Status::Running)
            }

            Op::Lt => self.infix_op(try_lt).map_err(FrameError::from),
            Op::Le => self.infix_op(try_le).map_err(FrameError::from),

            Op::Call(count) => {
                let value = self.pop_stack()?;
                let args = self.grab_args(count)?;
                if let Value::Builtin(f) = value {
                    let value = f(&args)?;
                    self.push_stack(value);
                    return Ok(Status::Running);
                }

                let object = value.as_object()?;
                let callee = object.try_borrow().map_err(|_| RuntimeError::BorrowError)?;

                if let Obj::Function(function) = &*callee {
                    if function.arity() != count {
                        return Err(FrameError::from(RuntimeError::ArityError));
                    }
                    self.local.extend(args);
                    let (ret, frame) = self.run(function)?;
                    self.restore_local(frame.bp())?;
                    self.push_stack(ret);
                    Ok(Status::Running)
                } else {
                    Err(FrameError::from(RuntimeError::TypeError))
                }
            }
            Op::AddLocal => {
                let value = self.pop_stack()?;
                self.local.push(value);
                Ok(Status::Running)
            }
            Op::AddGlobal => {
                let value = self.pop_stack()?;
                self.global.push(value);
                Ok(Status::Running)
            }
            Op::Load(at) => {
                let value = self
                    .local
                    .get(frame.bp() + at)
                    .ok_or(RuntimeError::LocalNotFound)?;
                self.push_stack(value.clone());
                Ok(Status::Running)
            }
            Op::Store(at) => {
                let value = self.pop_stack()?;
                let local = self
                    .local
                    .get_mut(frame.bp() + at)
                    .ok_or(RuntimeError::LocalNotFound)?;
                *local = value;
                Ok(Status::Running)
            }
            Op::Restore(count) => {
                let lp = self.lp() - count;
                self.restore_local(lp)?;
                Ok(Status::Running)
            }
            Op::LoadGlobal(at) => {
                let value = self.global.get(at).ok_or(RuntimeError::GlobalNotFound)?;
                self.push_stack(value.clone());
                Ok(Status::Running)
            }
            Op::StoreGlobal(at) => {
                let value = self.pop_stack()?;
                let global = self
                    .global
                    .get_mut(at)
                    .ok_or(RuntimeError::GlobalNotFound)?;
                *global = value;
                Ok(Status::Running)
            }
            Op::Jmp(ip) => {
                frame.jump(ip);
                Ok(Status::Running)
            }
            Op::JmpUnless(ip) => {
                let cond: bool = self.pop_stack()?.try_into()?;
                if !cond {
                    frame.jump(ip);
                }
                Ok(Status::Running)
            }
            Op::Ret => {
                let ret = self.pop_stack()?;
                Ok(Status::Finished(ret))
            }
        }
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum RuntimeError {
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
    #[error("no instruction to execute")]
    NoInstruction,
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
