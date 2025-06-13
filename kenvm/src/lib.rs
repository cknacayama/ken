pub mod builtin;
pub mod bytecode;
pub mod obj;
pub mod value;

use kenspan::Span;

use crate::bytecode::Op;
use crate::obj::{Function, Obj};
use crate::value::{Convert, Value};

#[derive(Debug, Clone)]
pub enum Status {
    Running,
    Finished(Value),
}

impl Status {
    #[must_use]
    pub const fn is_running(&self) -> bool {
        matches!(self, Self::Running)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Frame<'a> {
    function: &'a Function,
    ip:       u16,
    bp:       u32,
}

impl<'a> Frame<'a> {
    #[must_use]
    pub const fn new(function: &'a Function, bp: u32) -> Self {
        Self {
            function,
            bp,
            ip: 0,
        }
    }

    #[must_use]
    pub fn fetch_span(&self) -> Span {
        self.function.chunk().fetch_span(self.ip - 1)
    }

    pub fn fetch(&mut self) -> Option<Op> {
        let op = self.function.chunk().fetch(self.ip);
        self.ip += 1;
        op
    }

    #[must_use]
    pub fn fetch_value(&self, i: u16) -> Option<Value> {
        self.function.chunk().fetch_value(i)
    }

    pub const fn jump(&mut self, i: u16) {
        self.ip = i;
    }

    #[must_use]
    pub const fn bp(&self) -> u32 {
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
    #[allow(
        clippy::cast_possible_truncation,
        reason = "stack size is never greater than u32::MAX"
    )]
    const fn lp(&self) -> u32 {
        self.local.len() as u32
    }

    fn restore_local(&mut self, lp: u32) -> RuntimeResult<()> {
        if lp > self.lp() {
            return Err(RuntimeError::StackUnderflow);
        }
        let lp = lp as usize;
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

    fn restore_all(&mut self, sp: usize, lp: u32) -> RuntimeResult<()> {
        self.restore_stack(sp)?;
        self.restore_local(lp)?;
        Ok(())
    }

    pub fn run<'a>(&mut self, function: &'a Function) -> FrameResult<(Value, Frame<'a>)> {
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
        let bp = lp - u32::from(function.arity());
        Frame::new(function, bp)
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn add_local(&mut self, value: Value) -> RuntimeResult<()> {
        if self.local.len() >= u32::MAX as usize {
            return Err(RuntimeError::StackOverflow);
        }
        self.local.push(value);
        Ok(())
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
            Op::Push(i) => {
                let value = frame.fetch_value(i).ok_or(RuntimeError::NoValue)?;
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
                        self.add_local(arg)?;
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
                self.add_local(value)?;
            }
            Op::Load(i) => {
                let bp = frame.bp() as usize;
                let i = bp + i as usize;
                let value = self.load(i)?;
                self.push_stack(value);
            }
            Op::Store(i) => {
                let value = self.pop_stack()?;
                let bp = frame.bp() as usize;
                let i = bp + i as usize;
                self.store(i, value)?;
            }
            Op::Restore(count) => {
                let lp = self.lp() - u32::from(count);
                self.restore_local(lp)?;
            }
            Op::LoadGlobal(i) => {
                let value = self.load_global(i as usize)?;
                self.push_stack(value);
            }
            Op::StoreGlobal(i) => {
                let value = self.pop_stack()?;
                self.store_global(i as usize, value)?;
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
    #[error("stack overflow")]
    StackOverflow,
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
