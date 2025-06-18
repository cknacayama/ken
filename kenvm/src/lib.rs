pub mod builtin;
pub mod bytecode;
pub mod hash;
pub mod obj;
pub mod ty;
pub mod value;

use std::rc::Rc;

use kenspan::Span;

use crate::builtin::Builtin;
use crate::bytecode::{Chunk, Fetch, Op, OpStream};
use crate::hash::{HashValue, Table};
use crate::obj::{Function, MutObj, Obj};
use crate::ty::{Ty, TyId, Typed};
use crate::value::{Value, try_le, try_lt, try_pow};

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

    fn fetch<T>(&mut self) -> RuntimeResult<T>
    where
        OpStream<'a>: Fetch<T>,
    {
        self.stream.fetch().ok_or(RuntimeError::FetchError)
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
    types:  Vec<Rc<Ty>>,
    stack:  Vec<Value>,
    local:  Vec<Value>,
    global: Vec<Value>,
}

impl Default for Vm {
    fn default() -> Self {
        let global = Builtin::core_builtins()
            .iter()
            .copied()
            .map(|b| Value::Obj(Rc::new(Obj::Builtin(b))))
            .collect();
        let types = (0..TyId::OBJ.get())
            .map(|id| {
                let id = TyId::new(id);
                let data = id.default_data();
                Rc::new(data)
            })
            .collect();
        Self {
            global,
            types,
            stack: Vec::new(),
            local: Vec::new(),
        }
    }
}

impl Vm {
    #[must_use]
    pub fn get_type(&self, id: TyId) -> Option<&Rc<Ty>> {
        self.types.get(id.get())
    }

    pub fn get_value_type<T: Typed>(&self, v: &T) -> Option<&Rc<Ty>> {
        self.get_type(v.ty_id())
    }

    #[must_use]
    const fn sp(&self) -> usize {
        self.stack.len()
    }

    #[must_use]
    const fn lp(&self) -> usize {
        self.local.len()
    }

    fn restore_local(&mut self, lp: usize) {
        self.local.truncate(lp);
    }

    fn restore_stack(&mut self, sp: usize) -> RuntimeResult<()> {
        if sp > self.sp() {
            return Err(RuntimeError::StackUnderflow);
        }
        self.stack.truncate(sp);
        Ok(())
    }

    fn restore_all(&mut self, sp: usize, lp: usize) -> RuntimeResult<()> {
        self.restore_local(lp);
        self.restore_stack(sp)?;
        Ok(())
    }

    pub fn eval(&mut self, function: &Function) -> FrameResult<Value> {
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

        Ok(ret)
    }

    const fn create_frame<'a>(&self, function: &'a Function) -> Frame<'a> {
        let bp = self.lp() - function.arity();
        Frame::new(function.chunk(), bp)
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn local_view(&self, count: usize) -> RuntimeResult<&[Value]> {
        let lp = self.lp();
        if lp < count {
            return Err(RuntimeError::StackUnderflow);
        }
        let view = &self.local[lp - count..];
        Ok(view)
    }

    fn set_args(&mut self, count: usize) -> RuntimeResult<()> {
        match count {
            0 => {}
            1 => {
                let arg = self.pop_stack()?;
                self.local.push(arg);
            }
            count => {
                let sp = self.sp();
                if sp < count {
                    return Err(RuntimeError::StackUnderflow);
                }
                let args = self.stack.drain(sp - count..);
                self.local.extend(args);
            }
        }
        Ok(())
    }

    fn drain(&mut self, len: usize) -> RuntimeResult<std::vec::Drain<'_, Value>> {
        if self.sp() < len {
            return Err(RuntimeError::StackUnderflow);
        }
        Ok(self.stack.drain(self.stack.len() - len..))
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

    fn load_idx(&mut self) -> RuntimeResult<Status> {
        let idx = self.pop_stack()?;
        let indexed = self.pop_stack()?;
        match (indexed, idx) {
            (Value::MutObj(obj), Value::Int(idx)) => {
                let obj = obj.borrow()?;
                let item = match &*obj {
                    MutObj::List(values) => values
                        .get(usize::try_from(idx).map_err(|_| RuntimeError::OutOfBoundsInteger)?)
                        .ok_or(RuntimeError::OutOfBounds)?,
                    MutObj::Tuple(values) => values
                        .get(usize::try_from(idx).map_err(|_| RuntimeError::OutOfBoundsInteger)?)
                        .ok_or(RuntimeError::OutOfBounds)?,
                    MutObj::Table(table) => table
                        .get(&HashValue::Int(idx))
                        .ok_or(RuntimeError::NoValue)?,
                    _ => return Err(RuntimeError::TypeError),
                };
                self.push_stack(item.clone());

                Ok(Status::Running)
            }
            (Value::MutObj(obj), idx) => {
                let table = obj.as_table()?;
                let item = table.try_get(&idx)?;
                self.push_stack(item.clone());
                Ok(Status::Running)
            }
            _ => Err(RuntimeError::TypeError),
        }
    }

    fn store_idx(&mut self) -> RuntimeResult<Status> {
        let idx = self.pop_stack()?;
        let indexed = self.pop_stack()?;
        let value = self.pop_stack()?;
        match (indexed, idx) {
            (Value::MutObj(obj), Value::Int(idx)) => {
                let mut obj = obj.mut_borrow()?;
                let item = match &mut *obj {
                    MutObj::List(values) => values
                        .get_mut(
                            usize::try_from(idx).map_err(|_| RuntimeError::OutOfBoundsInteger)?,
                        )
                        .ok_or(RuntimeError::OutOfBounds)?,
                    MutObj::Tuple(values) => values
                        .get_mut(
                            usize::try_from(idx).map_err(|_| RuntimeError::OutOfBoundsInteger)?,
                        )
                        .ok_or(RuntimeError::OutOfBounds)?,
                    MutObj::Table(table) => table
                        .get_mut(&HashValue::Int(idx))
                        .ok_or(RuntimeError::NoValue)?,
                    _ => return Err(RuntimeError::TypeError),
                };
                *item = value;

                Ok(Status::Running)
            }
            (Value::MutObj(obj), idx) => {
                let mut table = obj.as_table_mut()?;
                let item = table.try_get_mut(&idx)?;
                *item = value;
                Ok(Status::Running)
            }
            _ => Err(RuntimeError::TypeError),
        }
    }

    fn call(&mut self, obj: &Obj, count: usize) -> FrameResult<Value> {
        match obj {
            Obj::Function(function) => {
                if function.arity() != count {
                    return Err(FrameError::from(RuntimeError::ArityError));
                }
                self.eval(function)
            }
            Obj::Builtin(builtin) => {
                let args = self.local_view(count)?;
                let ret = builtin(self, args)?;
                self.restore_local(self.lp() - count);
                Ok(ret)
            }
            _ => Err(FrameError::from(RuntimeError::TypeError)),
        }
    }

    fn call_method(&mut self, count: usize, name: Value) -> FrameResult<Status> {
        let name = name.as_obj()?.as_string().ok_or(RuntimeError::TypeError)?;
        let count = count + 1;
        self.set_args(count)?;
        let value = self.local.get(self.lp() - count).unwrap().clone();
        let ty = self.get_value_type(&value).unwrap().clone();
        let method = ty.get_method(name).ok_or(RuntimeError::MethodNotFound)?;
        let ret = self.call(method, count)?;
        self.push_stack(ret);
        Ok(Status::Running)
    }

    fn eval_next(&mut self, frame: &mut Frame<'_>) -> FrameResult<Status> {
        let op = frame.fetch()?;

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

            Op::PushBool(b) => {
                let value = Value::Bool(b);
                self.push_stack(value);
                Ok(Status::Running)
            }

            Op::PushU32(int) => {
                let value = Value::Int(i64::from(int));
                self.push_stack(value);
                Ok(Status::Running)
            }

            Op::MakeList(len) => {
                let values = self.drain(len)?.collect();
                let list = MutObj::List(values);
                let list = Value::from(list);
                self.push_stack(list);
                Ok(Status::Running)
            }

            Op::MakeTuple(len) => {
                let values = self.drain(len)?.collect();
                let list = MutObj::Tuple(values);
                let list = Value::from(list);
                self.push_stack(list);
                Ok(Status::Running)
            }

            Op::MakeTable(len) => {
                let values = self.drain(len * 2)?.collect::<Vec<_>>();
                let mut table = Table::new();
                for entry in values.chunks_exact(2) {
                    let [k, v] = entry else { unreachable!() };
                    let k = k.as_hash().ok_or(RuntimeError::NotHash)?;
                    table.insert(k, v.clone());
                }
                let table = MutObj::Table(table);
                let table = Value::from(table);
                self.push_stack(table);
                Ok(Status::Running)
            }

            Op::Add => self.infix_op(|a, b| a + b).map_err(FrameError::from),
            Op::Sub => self.infix_op(|a, b| a - b).map_err(FrameError::from),
            Op::Mul => self.infix_op(|a, b| a * b).map_err(FrameError::from),
            Op::Div => self.infix_op(|a, b| a / b).map_err(FrameError::from),
            Op::Rem => self.infix_op(|a, b| a % b).map_err(FrameError::from),
            Op::Pow => self.infix_op(try_pow).map_err(FrameError::from),

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
                let count = count as usize;
                self.set_args(count)?;
                let value = self.pop_stack()?;

                let obj = value.as_obj()?;
                let value = self.call(obj, count)?;
                self.push_stack(value);
                Ok(Status::Running)
            }
            Op::CallMethod(count, name) => {
                let name = frame.fetch_value(name).ok_or(RuntimeError::NoValue)?;
                self.call_method(count as usize, name)
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
            Op::LoadIdx => self.load_idx().map_err(FrameError::from),
            Op::StoreIdx => self.store_idx().map_err(FrameError::from),
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
                self.restore_local(lp);
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
            Op::JmpIfNot(ip) => {
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
    #[error("out of bounds integer value")]
    OutOfBoundsInteger,
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
    #[error("assertion failed")]
    AssertFailed,
    #[error("no value")]
    NoValue,
    #[error("method not found")]
    MethodNotFound,
    #[error("not hashable value")]
    NotHash,
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
