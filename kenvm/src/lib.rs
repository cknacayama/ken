pub mod arith;
pub mod builtin;
pub mod bytecode;
pub mod hash;
pub mod obj;
pub mod ty;
pub mod value;

use std::rc::Rc;

use kenspan::Span;

use crate::builtin::Builtin;
use crate::bytecode::{Chunk, Op, OpStream};
use crate::hash::{HashValue, Table};
use crate::obj::{Function, MutObj, Obj};
use crate::ty::{Ty, TyId, Typed};
use crate::value::Value;

#[derive(Debug, Clone)]
enum Status {
    Running,
    Finished,
}

#[derive(Debug, Clone)]
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
    types:  Vec<Rc<Ty>>,
    stack:  Vec<Value>,
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

    #[inline]
    fn restore(&mut self, bp: usize) {
        self.stack.truncate(bp);
    }

    pub fn eval(&mut self, function: &Function) -> FrameResult<Value> {
        self.eval_function(function)?;
        let value = self.pop_stack()?;
        Ok(value)
    }

    fn eval_function(&mut self, function: &Function) -> FrameResult<()> {
        let sp = self.sp();

        let mut frame = self.create_frame(function);
        loop {
            match self.eval_next(&mut frame) {
                Ok(Status::Running) => {}
                Ok(Status::Finished) => break Ok(()),
                Err(mut err) => {
                    let span = frame.stream.fetch_span().unwrap_or_default();
                    self.restore(sp);
                    err.spans.push(span);
                    break Err(err);
                }
            }
        }
    }

    const fn create_frame<'a>(&self, function: &'a Function) -> Frame<'a> {
        let bp = self.sp() - function.arity();
        Frame::new(function.chunk(), bp)
    }

    fn swap_remove(&mut self, at: usize) -> RuntimeResult<Value> {
        if at >= self.sp() {
            return Err(RuntimeError::StackUnderflow);
        }
        Ok(self.stack.swap_remove(at))
    }

    const fn swap_stack(&mut self, i: usize, j: usize) -> RuntimeResult<()> {
        let sp = self.sp();
        if i >= sp || j >= sp {
            return Err(RuntimeError::StackUnderflow);
        }
        self.stack.as_mut_slice().swap(i, j);
        Ok(())
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn local_view(&self, count: usize) -> RuntimeResult<&[Value]> {
        let sp = self.sp();
        if sp < count {
            return Err(RuntimeError::StackUnderflow);
        }
        let view = &self.stack[sp - count..];
        Ok(view)
    }

    fn drain(&mut self, len: usize) -> RuntimeResult<std::vec::Drain<'_, Value>> {
        if self.sp() < len {
            return Err(RuntimeError::StackUnderflow);
        }
        Ok(self.stack.drain(self.stack.len() - len..))
    }

    #[inline(never)]
    const fn load_stack(&self, offset: usize) -> RuntimeResult<&Value> {
        let sp = self.sp();
        if offset >= sp {
            return Err(RuntimeError::StackUnderflow);
        }
        Ok(&self.stack.as_slice()[sp - offset - 1])
    }

    #[inline]
    fn store_stack(&mut self, offset: usize, value: Value) -> RuntimeResult<()> {
        let sp = self.sp();
        if offset >= sp {
            return Err(RuntimeError::StackUnderflow);
        }
        self.stack[sp - offset - 1] = value;
        Ok(())
    }

    fn prefix_op(
        &mut self,
        op: impl FnOnce(&Value) -> RuntimeResult<Value>,
    ) -> RuntimeResult<Status> {
        let value = self.load_stack(0).and_then(op)?;
        let _ = self.store_stack(0, value);
        Ok(Status::Running)
    }

    fn infix_op(
        &mut self,
        op: impl FnOnce(&Value, &Value) -> RuntimeResult<Value>,
    ) -> RuntimeResult<Status> {
        let [lhs, rhs] = self.local_view(2)? else {
            unreachable!()
        };
        let value = op(lhs, rhs)?;
        let _ = self.pop_stack();
        let _ = self.store_stack(0, value);
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

    fn call(&mut self, obj: &Obj, count: usize) -> FrameResult<()> {
        match obj {
            Obj::Function(function) => {
                if function.arity() != count {
                    return Err(Box::from(RuntimeError::ArityError));
                }
                self.eval_function(function)
            }
            Obj::Builtin(builtin) => {
                let args = self.local_view(count)?;
                let ret = builtin(self, args)?;
                self.restore(self.sp() - count);
                self.push_stack(ret);
                Ok(())
            }
            _ => Err(Box::from(RuntimeError::TypeError)),
        }
    }

    fn call_method(&mut self, name: &Value, count: usize) -> FrameResult<()> {
        let name = name.as_obj()?.as_string().ok_or(RuntimeError::TypeError)?;
        let value = self.load_stack(count)?;
        let ty = self.get_value_type(value).unwrap().clone();
        let method = ty.get_method(name).ok_or(RuntimeError::MethodNotFound)?;
        self.call(method, count + 1)
    }

    fn eval_next(&mut self, frame: &mut Frame) -> FrameResult<Status> {
        let op = frame.stream.fetch().ok_or(RuntimeError::FetchError)?;

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

            Op::Add => self.infix_op(|a, b| a + b).map_err(Box::from),
            Op::Sub => self.infix_op(|a, b| a - b).map_err(Box::from),
            Op::Mul => self.infix_op(|a, b| a * b).map_err(Box::from),
            Op::Div => self.infix_op(|a, b| a / b).map_err(Box::from),
            Op::Rem => self.infix_op(|a, b| a % b).map_err(Box::from),
            Op::Pow => self.infix_op(Value::pow).map_err(Box::from),

            Op::Neg => self.prefix_op(|x| -x).map_err(Box::from),
            Op::Not => self.prefix_op(|x| !x).map_err(Box::from),

            Op::Eq => {
                let [lhs, rhs] = self.local_view(2)? else {
                    unreachable!()
                };
                let value = Value::Bool(lhs == rhs);
                let _ = self.pop_stack();
                let _ = self.store_stack(0, value);
                Ok(Status::Running)
            }

            Op::Lt => self.infix_op(Value::lt).map_err(Box::from),
            Op::Le => self.infix_op(Value::le).map_err(Box::from),

            Op::Call(count) => {
                let count = count as usize;
                let value = self.load_stack(count)?;

                let obj = value.as_obj()?.clone();
                self.call(&obj, count)?;
                let _ = self.swap_remove(self.sp() - 2);
                Ok(Status::Running)
            }
            Op::CallMethod(count, name) => {
                let name = frame.fetch_value(name).ok_or(RuntimeError::NoValue)?;
                self.call_method(&name, count as usize)?;
                Ok(Status::Running)
            }
            Op::AddGlobal => {
                let value = self.pop_stack()?;
                self.global.push(value);
                Ok(Status::Running)
            }
            Op::LoadIdx => self.load_idx().map_err(Box::from),
            Op::StoreIdx => self.store_idx().map_err(Box::from),
            Op::Load(at) => {
                let abs = frame.bp() + at;
                if abs >= self.sp() {
                    return Err(Box::from(RuntimeError::LocalNotFound));
                }

                let value = self.stack.as_slice()[abs].clone();
                self.push_stack(value);
                Ok(Status::Running)
            }
            Op::Store(at) => {
                let value = self.pop_stack()?;
                let bp = frame.bp();
                let local = self
                    .stack
                    .get_mut(bp + at)
                    .ok_or(RuntimeError::LocalNotFound)?;
                *local = value;
                Ok(Status::Running)
            }
            Op::Restore(count) => {
                let value = self.pop_stack()?;
                if count > self.sp() {
                    return Err(Box::from(RuntimeError::StackUnderflow));
                }
                let lp = self.sp() - count;
                self.restore(lp);
                self.push_stack(value);
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
            Op::JmpIfNot(ip) => match self.pop_stack()? {
                Value::Bool(false) => {
                    frame.jump(ip);
                    Ok(Status::Running)
                }
                Value::Bool(true) => Ok(Status::Running),
                _ => Err(Box::from(RuntimeError::TypeError)),
            },
            Op::Ret => {
                self.swap_stack(self.sp() - 1, frame.bp())?;
                self.restore(frame.bp() + 1);
                Ok(Status::Finished)
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

#[derive(Debug)]
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

impl From<RuntimeError> for Box<FrameError> {
    fn from(value: RuntimeError) -> Self {
        Self::new(FrameError {
            runtime: value,
            spans:   Vec::new(),
        })
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type FrameResult<T> = Result<T, Box<FrameError>>;

#[cfg(test)]
mod test {
    use super::*;
    use crate::bytecode::ChunkBuilder;

    #[test]
    fn test_add() {
        let mut vm = Vm::default();
        let mut builder = ChunkBuilder::new();
        let span = Span::default();
        builder.push_op(Op::PushU32(1), span);
        builder.push_op(Op::PushU32(2), span);
        builder.push_op(Op::Add, span);
        builder.push_op(Op::Ret, span);
        let chunk = builder.finish();
        let function = Function::new(None, 0, chunk);
        let result = vm.eval(&function).unwrap();
        assert_eq!(result, Value::Int(3));
    }
}
