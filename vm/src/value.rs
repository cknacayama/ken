use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::Chunk;
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(f64),
    Builtin(Builtin),
    Object(ObjectRef),
}

impl Value {
    pub fn as_object(&self) -> RuntimeResult<ObjectRef> {
        if let Self::Object(v) = self {
            Ok(v.clone())
        } else {
            Err(RuntimeError::TypeError)
        }
    }
}

pub type ObjectRef = Rc<RefCell<Object>>;

#[derive(Debug)]
pub enum Object {
    List(Vec<Value>),
    Function(Function),
    Closure(Closure),
}

#[derive(Debug, Clone, Copy)]
pub struct Builtin(fn(&[Value]) -> RuntimeResult<Value>);

impl Builtin {
    pub const fn new(f: fn(&[Value]) -> RuntimeResult<Value>) -> Self {
        Self(f)
    }
}

impl std::ops::Deref for Builtin {
    type Target = fn(&[Value]) -> RuntimeResult<Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct Function {
    arity: u8,
    chunk: Chunk,
}

impl Function {
    #[must_use]
    pub const fn new(arity: u8, chunk: Chunk) -> Self {
        Self { arity, chunk }
    }

    #[must_use]
    pub const fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    #[must_use]
    pub const fn arity(&self) -> u8 {
        self.arity
    }
}

#[derive(Debug)]
pub struct Closure {
    captures: Vec<Value>,
    function: Function,
}
