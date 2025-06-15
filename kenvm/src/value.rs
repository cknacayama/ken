#![allow(clippy::cast_precision_loss)]
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::obj::{Function, ObjRef};
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Builtin(Builtin),
    Obj(ObjRef),
}

impl Value {
    pub const fn as_object(&self) -> RuntimeResult<&ObjRef> {
        if let Self::Obj(v) = self {
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
            Self::Builtin(builtin) => write!(f, "{builtin}"),
            Self::Obj(obj) => {
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

macro_rules! infix_impl {
    ($trayt:ident::$op:ident) => {
        impl $trayt for Value {
            type Output = RuntimeResult<Value>;

            #[inline]
            fn $op(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int($trayt::$op(lhs, rhs))),
                    (Value::Int(lhs), Value::Float(rhs)) => {
                        Ok(Value::Float($trayt::$op(lhs as f64, rhs)))
                    }
                    (Value::Float(lhs), Value::Int(rhs)) => {
                        Ok(Value::Float($trayt::$op(lhs, rhs as f64)))
                    }
                    (Value::Float(lhs), Value::Float(rhs)) => {
                        Ok(Value::Float($trayt::$op(lhs, rhs)))
                    }
                    _ => Err(RuntimeError::TypeError),
                }
            }
        }
    };
}

macro_rules! value_impl {
    ($val:ty, $variant:ident) => {
        impl From<$val> for Value {
            #[inline]
            fn from(value: $val) -> Self {
                Self::$variant(value)
            }
        }

        impl TryFrom<Value> for $val {
            type Error = RuntimeError;

            #[inline]
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
            #[inline]
            fn from(value: $val) -> Self {
                use std::cell::RefCell;

                use crate::obj::Obj;

                let obj = Obj::$variant(value);
                Self::Obj(Rc::new(RefCell::new(obj)))
            }
        }
    };
}

value_impl!(f64, Float);
value_impl!(i64, Int);
value_impl!(bool, Bool);
value_impl!(ObjRef, Obj);
value_impl!(Builtin, Builtin);
value_impl!(Function, obj Function);

infix_impl!(Add::add);
infix_impl!(Sub::sub);
infix_impl!(Mul::mul);

impl Div for Value {
    type Output = RuntimeResult<Self>;

    #[inline]
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs
                .checked_div(rhs)
                .ok_or(RuntimeError::DivisionByZero)
                .map(Value::Int),
            (Self::Int(lhs), Self::Float(rhs)) => Ok(Self::Float(lhs as f64 / rhs)),
            (Self::Float(lhs), Self::Int(rhs)) => Ok(Self::Float(lhs / rhs as f64)),
            (Self::Float(lhs), Self::Float(rhs)) => Ok(Self::Float(lhs / rhs)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Neg for Value {
    type Output = RuntimeResult<Self>;

    #[inline]
    fn neg(self) -> Self::Output {
        match self {
            Self::Int(int) => Ok(Self::Int(-int)),
            Self::Float(float) => Ok(Self::Float(-float)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Not for Value {
    type Output = RuntimeResult<Self>;

    #[inline]
    fn not(self) -> Self::Output {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

#[inline]
pub(crate) fn try_lt(lhs: Value, rhs: Value) -> RuntimeResult<Value> {
    let result = match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => lhs < rhs,
        (Value::Int(lhs), Value::Float(rhs)) => (lhs as f64) < rhs,
        (Value::Float(lhs), Value::Int(rhs)) => lhs < rhs as f64,
        (Value::Float(lhs), Value::Float(rhs)) => lhs < rhs,
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Bool(result))
}

#[inline]
pub(crate) fn try_le(lhs: Value, rhs: Value) -> RuntimeResult<Value> {
    let result = match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => lhs <= rhs,
        (Value::Int(lhs), Value::Float(rhs)) => (lhs as f64) <= rhs,
        (Value::Float(lhs), Value::Int(rhs)) => lhs <= rhs as f64,
        (Value::Float(lhs), Value::Float(rhs)) => lhs <= rhs,
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Bool(result))
}
