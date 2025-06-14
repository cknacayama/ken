pub mod builtin;
pub mod bytecode;
pub mod obj;
pub mod value;

use kenspan::Span;

use crate::bytecode::{Op, OpCode};
use crate::obj::{Function, Obj};
use crate::value::{Convert, Value};

#[derive(Debug, Clone)]
enum Status {
    Running,
    Finished(Value),
}

#[derive(Debug, Clone, Copy)]
struct Frame<'a> {
    function: &'a Function,
    ip:       usize,
    bp:       usize,
}

impl<'a> Frame<'a> {
    #[must_use]
    const fn new(function: &'a Function, bp: usize) -> Self {
        Self {
            function,
            bp,
            ip: 0,
        }
    }

    #[must_use]
    fn fetch_span(&self) -> Span {
        self.function.chunk().fetch_span(self.ip - 1)
    }

    #[must_use]
    fn fetch_u8(&mut self) -> Option<u8> {
        let byte = self.function.chunk().fetch_u8(self.ip)?;
        self.ip += 1;
        Some(byte)
    }

    #[must_use]
    fn fetch_usize(&mut self) -> Option<usize> {
        let word = self.function.chunk().fetch_usize(self.ip)?;
        self.ip += 8;
        Some(word)
    }

    fn fetch(&mut self) -> Option<Op> {
        let code = self.fetch_u8().and_then(OpCode::decode)?;

        let op = match code {
            OpCode::Nop => Op::Nop,
            OpCode::Pop => Op::Pop,
            OpCode::Push => {
                let arg = self.fetch_usize()?;
                Op::Push(arg)
            }
            OpCode::Neg => Op::Neg,
            OpCode::Not => Op::Not,
            OpCode::Add => Op::Add,
            OpCode::Sub => Op::Sub,
            OpCode::Mul => Op::Mul,
            OpCode::Div => Op::Div,
            OpCode::Call => {
                let arg = self.fetch_u8()?;
                Op::Call(arg)
            }
            OpCode::AddLocal => Op::AddLocal,
            OpCode::Load => {
                let arg = self.fetch_usize()?;
                Op::Load(arg)
            }
            OpCode::Store => {
                let arg = self.fetch_usize()?;
                Op::Load(arg)
            }
            OpCode::Restore => {
                let arg = self.fetch_usize()?;
                Op::Restore(arg)
            }
            OpCode::LoadGlobal => {
                let arg = self.fetch_usize()?;
                Op::LoadGlobal(arg)
            }
            OpCode::StoreGlobal => {
                let arg = self.fetch_usize()?;
                Op::StoreGlobal(arg)
            }
            OpCode::Jmp => {
                let arg = self.fetch_usize()?;
                Op::Jmp(arg)
            }
            OpCode::JmpUnless => {
                let arg = self.fetch_usize()?;
                Op::JmpUnless(arg)
            }
            OpCode::Ret => Op::Ret,
        };

        Some(op)
    }

    #[must_use]
    fn fetch_value(&self, i: usize) -> Option<Value> {
        self.function.chunk().fetch_value(i)
    }

    const fn jump(&mut self, i: usize) {
        self.ip = i;
    }

    #[must_use]
    const fn bp(&self) -> usize {
        self.bp
    }
}

pub struct Vm {
    debug:   bool,
    stack:   Vec<Value>,
    local:   Vec<Value>,
    globals: Vec<Value>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            debug:   false,
            stack:   Vec::new(),
            local:   Vec::new(),
            globals: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_debug(self) -> Self {
        Self {
            debug: true,
            ..self
        }
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
        let lp = lp;
        let _ = self.local.drain(lp..);
        Ok(())
    }

    fn restore_stack(&mut self, sp: usize) -> RuntimeResult<()> {
        if sp > self.sp() {
            return Err(RuntimeError::StackUnderflow);
        }
        let _ = self.stack.drain(sp..);
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
            if self.debug {
                self.debug();
            }
            match self.eval_next(&mut frame) {
                Ok(Status::Running) => {}
                Ok(Status::Finished(ret)) => break ret,
                Err(mut err) => {
                    let span = frame.fetch_span();
                    let _ = self.restore_all(sp, lp);
                    err.spans.push(span);
                    return Err(err);
                }
            }
        };

        Ok((ret, frame))
    }

    fn debug(&self) {
        todo!()
    }

    fn create_frame<'a>(&self, function: &'a Function) -> Frame<'a> {
        let lp = self.lp();
        let bp = lp - usize::from(function.arity());
        Frame::new(function, bp)
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn add_local(&mut self, value: Value) {
        self.local.push(value);
    }

    fn store(&mut self, i: usize, value: Value) -> RuntimeResult<()> {
        let local = self.local.get_mut(i).ok_or(RuntimeError::LocalNotFound)?;
        *local = value;
        Ok(())
    }

    fn load(&self, i: usize) -> RuntimeResult<Value> {
        self.local
            .get(i)
            .ok_or(RuntimeError::LocalNotFound)
            .cloned()
    }

    fn store_global(&mut self, i: usize, value: Value) -> RuntimeResult<()> {
        let global = self
            .globals
            .get_mut(i)
            .ok_or(RuntimeError::GlobalNotFound)?;
        *global = value;
        Ok(())
    }

    fn load_global(&self, i: usize) -> RuntimeResult<Value> {
        self.globals
            .get(i)
            .ok_or(RuntimeError::GlobalNotFound)
            .cloned()
    }

    fn grab_args(&mut self, count: u8) -> RuntimeResult<Vec<Value>> {
        let count = usize::from(count);
        if self.sp() < count {
            return Err(RuntimeError::StackUnderflow);
        }
        let args = self.stack.drain(self.stack.len() - count..).collect();
        Ok(args)
    }

    fn prefix_op<T: Convert>(&mut self, f: impl FnOnce(T) -> T) -> RuntimeResult<()> {
        let value = self.pop_stack()?.try_into()?;
        let value = f(value);
        self.push_stack(value.into());
        Ok(())
    }

    fn infix_op<T: Convert>(&mut self, f: impl FnOnce(T, T) -> T) -> RuntimeResult<()> {
        let rhs = self.pop_stack()?.try_into()?;
        let lhs = self.pop_stack()?.try_into()?;
        let value = f(lhs, rhs);
        self.push_stack(value.into());
        Ok(())
    }

    fn try_infix_op<T: Convert>(
        &mut self,
        f: impl FnOnce(T, T) -> RuntimeResult<T>,
    ) -> RuntimeResult<()> {
        let rhs = self.pop_stack()?.try_into()?;
        let lhs = self.pop_stack()?.try_into()?;
        let value = f(lhs, rhs)?;
        self.push_stack(value.into());
        Ok(())
    }

    fn eval_next(&mut self, frame: &mut Frame<'_>) -> FrameResult<Status> {
        let op = frame.fetch().ok_or(RuntimeError::NoInstruction)?;

        let mut status = Status::Running;

        match op {
            Op::Nop => {}
            Op::Pop => {
                self.pop_stack()?;
            }
            Op::Push(at) => {
                let value = frame.fetch_value(at).ok_or(RuntimeError::NoValue)?;
                self.push_stack(value);
            }

            Op::Add => self.infix_op::<f64>(|a, b| a + b)?,
            Op::Sub => self.infix_op::<f64>(|a, b| a - b)?,
            Op::Mul => self.infix_op::<f64>(|a, b| a * b)?,
            Op::Div => self.try_infix_op::<f64>(|a, b| {
                if b == 0.0 {
                    Err(RuntimeError::DivisionByZero)
                } else {
                    Ok(a / b)
                }
            })?,

            Op::Neg => self.prefix_op::<f64>(|x| -x)?,
            Op::Not => self.prefix_op::<bool>(|x| !x)?,

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
                    for arg in args {
                        self.add_local(arg);
                    }
                    let (ret, frame) = self.run(function)?;
                    self.restore_local(frame.bp())?;
                    self.push_stack(ret);
                } else {
                    return Err(FrameError::from(RuntimeError::TypeError));
                }
            }
            Op::AddLocal => {
                let value = self.pop_stack()?;
                self.add_local(value);
            }
            Op::Load(at) => {
                let value = self.load(frame.bp() + at)?;
                self.push_stack(value);
            }
            Op::Store(at) => {
                let value = self.pop_stack()?;
                self.store(frame.bp() + at, value)?;
            }
            Op::Restore(count) => {
                let lp = self.lp() - count;
                self.restore_local(lp)?;
            }
            Op::LoadGlobal(at) => {
                let value = self.load_global(at)?;
                self.push_stack(value);
            }
            Op::StoreGlobal(at) => {
                let value = self.pop_stack()?;
                self.store_global(at, value)?;
            }
            Op::Jmp(ip) => {
                frame.jump(ip);
            }
            Op::JmpUnless(ip) => {
                let cond: bool = self.pop_stack()?.try_into()?;
                if !cond {
                    frame.jump(ip);
                }
            }
            Op::Ret => {
                let ret = self.pop_stack()?;
                status = Status::Finished(ret);
            }
        }

        Ok(status)
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
