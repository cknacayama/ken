use crate::bytecode::{Frame, Op};
use crate::value::{Function, Object, Value};

pub mod builtin;
pub mod bytecode;
pub mod value;

#[derive(Debug, Clone, Copy)]
pub enum RuntimeError {
    TypeError,
    ArityError,
    BorrowError,
    FrameUnderflow,
    StackUnderflow,
    StackOverflow,
    NoInstruction,
    NoValue,
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub enum Status {
    Running,
    Finished,
}

impl Status {
    #[must_use]
    pub const fn is_running(&self) -> bool {
        matches!(self, Self::Running)
    }
}

pub struct Vm {
    debug: bool,
    stack: Vec<Value>,
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
            debug: false,
            stack: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_debug(self) -> Self {
        Self {
            debug: true,
            ..self
        }
    }

    pub fn run(&mut self, function: &Function) -> RuntimeResult<()> {
        let debug = self.debug;

        let mut frame = self.create_frame(function);
        while self.eval_next(&mut frame)?.is_running() {
            if debug {
                self.debug();
            }
        }

        Ok(())
    }

    fn debug(&self) {
        todo!()
    }

    fn create_frame<'a>(&self, function: &'a Function) -> Frame<'a> {
        #[allow(
            clippy::cast_possible_truncation,
            reason = "stack size is never greater than u32::MAX"
        )]
        let sp = self.stack.len() as u32;
        let bp = sp - u32::from(function.arity());
        Frame::new(function, bp)
    }

    fn pop_stack(&mut self) -> RuntimeResult<Value> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn push_stack(&mut self, value: Value) -> RuntimeResult<()> {
        if self.stack.len() >= u32::MAX as usize {
            Err(RuntimeError::StackOverflow)
        } else {
            self.stack.push(value);
            Ok(())
        }
    }

    fn fetch_stack(&self, i: usize) -> RuntimeResult<Value> {
        let idx = self
            .stack
            .len()
            .checked_sub(i)
            .ok_or(RuntimeError::StackUnderflow)?;

        Ok(self.stack[idx].clone())
    }

    fn store(&mut self, i: usize, value: Value) -> RuntimeResult<()> {
        let local = self.stack.get_mut(i).ok_or(RuntimeError::StackUnderflow)?;
        *local = value;
        Ok(())
    }

    fn load(&self, i: usize) -> RuntimeResult<Value> {
        self.stack
            .get(i)
            .ok_or(RuntimeError::StackUnderflow)
            .cloned()
    }

    fn arg_slice(&self, count: u8) -> &[Value] {
        &self.stack[self.stack.len() - count as usize..]
    }

    fn eval_next(&mut self, frame: &mut Frame<'_>) -> RuntimeResult<Status> {
        let op = frame.fetch().ok_or(RuntimeError::NoInstruction)?;

        match op {
            Op::Nop => Ok(Status::Running),
            Op::Pop => {
                self.pop_stack()?;
                Ok(Status::Running)
            }
            Op::Push(i) => {
                let value = frame.fetch_value(i).ok_or(RuntimeError::NoValue)?;
                self.push_stack(value)?;
                Ok(Status::Running)
            }
            Op::Add => {
                let rhs = self.pop_stack()?;
                let lhs = self.pop_stack()?;

                let value = match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
                    _ => return Err(RuntimeError::TypeError),
                };
                self.push_stack(value)?;
                Ok(Status::Running)
            }
            Op::Call(count) => {
                let value = self.fetch_stack(count as usize + 1)?;
                if let Value::Builtin(f) = value {
                    let args = self.arg_slice(count);
                    let value = f(args)?;
                    self.push_stack(value)?;
                    return Ok(Status::Running);
                }
                let object = value.as_object()?;
                let callee = object.try_borrow().map_err(|_| RuntimeError::BorrowError)?;
                match &*callee {
                    Object::Function(function) => self.run(function).map(|()| Status::Running),
                    _ => Err(RuntimeError::TypeError),
                }
            }
            Op::Load(i) => {
                let bp = frame.bp() as usize;
                let i = bp + i as usize;
                let value = self.load(i)?;
                self.push_stack(value)?;
                Ok(Status::Running)
            }
            Op::Store(i) => {
                let value = self.pop_stack()?;
                let bp = frame.bp() as usize;
                let i = bp + i as usize;
                self.store(i, value)?;
                Ok(Status::Running)
            }
            Op::Exit => Ok(Status::Finished),
        }
    }
}
