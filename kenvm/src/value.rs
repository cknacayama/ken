#![allow(clippy::cast_precision_loss)]
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};
use std::rc::Rc;

use crate::hash::HashValue;
use crate::obj::{Function, MutObj, MutObjRef, Obj, ObjRef};
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

    #[must_use]
    pub fn as_hash(&self) -> Option<HashValue> {
        match self {
            Self::Unit => Some(HashValue::Unit),
            Self::Bool(b) => Some(HashValue::Bool(*b)),
            Self::Int(x) => Some(HashValue::Int(*x)),
            Self::Obj(obj) => match obj.as_ref() {
                Obj::String(s) => Some(HashValue::String(s.clone())),
                _ => None,
            },
            _ => None,
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
                    let ptr = Rc::as_ptr(obj);
                    write!(f, "<{obj} at {ptr:?}>")
                }
            }
        }
    }
}

macro_rules! infix_impl {
    ($trayt:ident::$op:ident) => {
        impl $trayt for Value {
            type Output = RuntimeResult<Self>;

            fn $op(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Self::Int(lhs), Self::Int(rhs)) => Ok(Self::Int($trayt::$op(lhs, rhs))),
                    (Self::Int(lhs), Self::Float(rhs)) => {
                        Ok(Self::Float($trayt::$op(lhs as f64, rhs)))
                    }
                    (Self::Float(lhs), Self::Int(rhs)) => {
                        Ok(Self::Float($trayt::$op(lhs, rhs as f64)))
                    }
                    (Self::Float(lhs), Self::Float(rhs)) => Ok(Self::Float($trayt::$op(lhs, rhs))),
                    _ => Err(RuntimeError::TypeError),
                }
            }
        }
        impl $trayt for &Value {
            type Output = RuntimeResult<Value>;

            fn $op(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int($trayt::$op(lhs, rhs))),
                    (Value::Int(lhs), Value::Float(rhs)) => {
                        Ok(Value::Float($trayt::$op(*lhs as f64, rhs)))
                    }
                    (Value::Float(lhs), Value::Int(rhs)) => {
                        Ok(Value::Float($trayt::$op(lhs, *rhs as f64)))
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
                Self::Obj(Rc::new(obj))
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
value_impl!(Rc<str>, obj String);
value_impl!(Vec<Value>, mut obj List);
value_impl!(Box<[Value]>, mut obj Tuple);

impl From<MutObj> for Value {
    fn from(value: MutObj) -> Self {
        let obj = MutObjRef::new(value);
        Self::MutObj(obj)
    }
}

infix_impl!(Add::add);
infix_impl!(Sub::sub);
infix_impl!(Mul::mul);

impl Div for Value {
    type Output = RuntimeResult<Self>;

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

impl Rem for Value {
    type Output = RuntimeResult<Self>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs
                .checked_rem(rhs)
                .ok_or(RuntimeError::DivisionByZero)
                .map(Value::Int),
            (Self::Int(lhs), Self::Float(rhs)) => Ok(Self::Float(lhs as f64 % rhs)),
            (Self::Float(lhs), Self::Int(rhs)) => Ok(Self::Float(lhs % rhs as f64)),
            (Self::Float(lhs), Self::Float(rhs)) => Ok(Self::Float(lhs % rhs)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Div for &Value {
    type Output = RuntimeResult<Value>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs
                .checked_div(*rhs)
                .ok_or(RuntimeError::DivisionByZero)
                .map(Value::Int),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 / rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Rem for &Value {
    type Output = RuntimeResult<Value>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs
                .checked_rem(*rhs)
                .ok_or(RuntimeError::DivisionByZero)
                .map(Value::Int),
            (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(*lhs as f64 % rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % *rhs as f64)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Neg for &Value {
    type Output = RuntimeResult<Value>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Int(int) => Ok(Value::Int(-int)),
            Value::Float(float) => Ok(Value::Float(-float)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

impl Not for &Value {
    type Output = RuntimeResult<Value>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(RuntimeError::TypeError),
        }
    }
}

pub(crate) fn try_pow(lhs: &Value, rhs: &Value) -> RuntimeResult<Value> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs.powf(*rhs))),
        (Value::Float(lhs), Value::Int(rhs)) => {
            let rhs = (*rhs)
                .try_into()
                .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
            Ok(Value::Float(lhs.powi(rhs)))
        }
        (Value::Int(lhs), Value::Float(rhs)) => {
            let lhs = *lhs as f64;
            Ok(Value::Float(lhs.powf(*rhs)))
        }
        (Value::Int(lhs), Value::Int(rhs @ 0..)) => {
            let rhs = (*rhs)
                .try_into()
                .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
            Ok(Value::Int(lhs.pow(rhs)))
        }
        (Value::Int(lhs), Value::Int(rhs)) => {
            let lhs = *lhs as f64;
            let rhs = (*rhs)
                .try_into()
                .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
            Ok(Value::Float(lhs.powi(rhs)))
        }
        _ => Err(RuntimeError::TypeError),
    }
}

pub(crate) fn try_lt(lhs: &Value, rhs: &Value) -> RuntimeResult<Value> {
    let result = match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => lhs < rhs,
        (Value::Int(lhs), Value::Float(rhs)) => &(*lhs as f64) < rhs,
        (Value::Float(lhs), Value::Int(rhs)) => lhs < &(*rhs as f64),
        (Value::Float(lhs), Value::Float(rhs)) => lhs < rhs,
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Bool(result))
}

pub(crate) fn try_le(lhs: &Value, rhs: &Value) -> RuntimeResult<Value> {
    let result = match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => lhs <= rhs,
        (Value::Int(lhs), Value::Float(rhs)) => &(*lhs as f64) <= rhs,
        (Value::Float(lhs), Value::Int(rhs)) => lhs <= &(*rhs as f64),
        (Value::Float(lhs), Value::Float(rhs)) => lhs <= rhs,
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Bool(result))
}
