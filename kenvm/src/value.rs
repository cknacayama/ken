use std::fmt::Display;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::obj::ObjRef;
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Number(f64),
    Builtin(Builtin),
    Object(ObjRef),
}

impl Value {
    pub fn as_object(&self) -> RuntimeResult<ObjRef> {
        if let Self::Object(v) = self {
            Ok(v.clone())
        } else {
            Err(RuntimeError::TypeError)
        }
    }
}

impl From<TypeError> for RuntimeError {
    fn from(_value: TypeError) -> Self {
        Self::TypeError
    }
}

pub struct TypeError;
pub trait Convert: TryFrom<Value, Error = TypeError> + Into<Value> {}

macro_rules! value_impl {
    ($val:ty, $variant:ident) => {
        impl From<$val> for Value {
            fn from(value: $val) -> Self {
                Self::$variant(value)
            }
        }

        impl TryFrom<Value> for $val {
            type Error = TypeError;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                if let Value::$variant(value) = value {
                    Ok(value)
                } else {
                    Err(TypeError)
                }
            }
        }

        impl Convert for $val {}
    };
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Number(x) => write!(f, "{x}"),
            Self::Builtin(builtin) => write!(f, "{builtin}"),
            Self::Object(obj) => {
                let ptr = Rc::as_ptr(obj);
                let obj = obj.borrow();
                if obj.is_pretty() {
                    write!(f, "{obj}")
                } else {
                    write!(f, "<{obj} at {ptr:?}>")
                }
            }
        }
    }
}

value_impl!(f64, Number);
value_impl!(bool, Bool);
value_impl!(ObjRef, Object);
value_impl!(Builtin, Builtin);
