use std::fmt::Display;

use crate::obj::{Function, MutObj, MutObjRef, Obj, ObjRef, StrRef};
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    MutObj(MutObjRef),
    Obj(ObjRef),
}

impl Value {
    pub const fn as_obj(&self) -> RuntimeResult<&ObjRef> {
        if let Self::Obj(v) = self {
            Ok(v)
        } else {
            Err(RuntimeError::TypeError)
        }
    }

    pub const fn as_mut_obj(&self) -> RuntimeResult<&MutObjRef> {
        if let Self::MutObj(v) = self {
            Ok(v)
        } else {
            Err(RuntimeError::TypeError)
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(x) => write!(f, "{x}"),
            Self::Float(x) => write!(f, "{x}"),
            Self::MutObj(obj) => {
                let ptr = obj.as_ptr();
                match obj.borrow() {
                    Ok(obj) if obj.is_pretty() => write!(f, "{obj}"),
                    Ok(obj) => write!(f, "<{obj} at {ptr:?}>"),
                    Err(_) => write!(f, "<mut obj at {ptr:?}>"),
                }
            }
            Self::Obj(obj) => {
                if obj.is_pretty() {
                    write!(f, "{obj}")
                } else {
                    let ptr = obj.as_ptr();
                    write!(f, "<{obj} at {ptr:?}>")
                }
            }
        }
    }
}

macro_rules! value_impl {
    ($val:ty, $variant:ident) => {
        impl From<$val> for Value {
            fn from(value: $val) -> Self {
                Self::$variant(value)
            }
        }

        impl TryFrom<Value> for $val {
            type Error = RuntimeError;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                if let Value::$variant(value) = value {
                    Ok(value)
                } else {
                    Err(RuntimeError::TypeError)
                }
            }
        }
    };

    ($val:ty, obj $variant:ident) => {
        impl From<$val> for Value {
            fn from(value: $val) -> Self {
                let obj = Obj::$variant(value);
                Self::Obj(ObjRef::new(obj))
            }
        }
    };

    ($val:ty, mut obj $variant:ident) => {
        impl From<$val> for Value {
            fn from(value: $val) -> Self {
                let obj = MutObj::$variant(value);
                Self::MutObj(MutObjRef::new(obj))
            }
        }
    };
}

value_impl!(f64, Float);
value_impl!(i64, Int);
value_impl!(bool, Bool);
value_impl!(MutObjRef, MutObj);
value_impl!(Function, obj Function);
value_impl!(StrRef, obj Str);
value_impl!(Vec<Value>, mut obj List);
value_impl!(Box<[Value]>, mut obj Tuple);

impl From<MutObj> for Value {
    fn from(value: MutObj) -> Self {
        let obj = MutObjRef::new(value);
        Self::MutObj(obj)
    }
}
