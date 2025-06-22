#![allow(clippy::cast_precision_loss)]
use std::ops::{Add, Div, Mul, Neg, Not, Rem, Sub};

use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

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

macro_rules! cmp_impl {
    ($($op:ident)*) => {
        impl Value {
            $(
                pub fn $op(&self, rhs: &Self) -> RuntimeResult<Self> {
                    match (self, rhs) {
                        (Self::Int(lhs), Self::Int(rhs)) => Ok(Self::Bool(lhs.$op(rhs))),
                        (Self::Int(lhs), Self::Float(rhs)) => Ok(Self::Bool((*lhs as f64).$op(rhs))),
                        (Self::Float(lhs), Self::Int(rhs)) => Ok(Self::Bool(lhs.$op(&(*rhs as f64)))),
                        (Self::Float(lhs), Self::Float(rhs)) => Ok(Self::Bool(lhs.$op(rhs))),
                        _ => Err(RuntimeError::TypeError),
                    }
                }
            )*
        }
    };
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

impl Value {
    pub fn pow(&self, rhs: &Self) -> RuntimeResult<Self> {
        match (self, rhs) {
            (Self::Float(lhs), Self::Float(rhs)) => Ok(Self::Float(lhs.powf(*rhs))),
            (Self::Float(lhs), Self::Int(rhs)) => {
                let rhs = (*rhs)
                    .try_into()
                    .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
                Ok(Self::Float(lhs.powi(rhs)))
            }
            (Self::Int(lhs), Self::Float(rhs)) => {
                let lhs = *lhs as f64;
                Ok(Self::Float(lhs.powf(*rhs)))
            }
            (Self::Int(lhs), Self::Int(rhs @ 0..)) => {
                let rhs = (*rhs)
                    .try_into()
                    .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
                Ok(Self::Int(lhs.pow(rhs)))
            }
            (Self::Int(lhs), Self::Int(rhs)) => {
                let lhs = *lhs as f64;
                let rhs = (*rhs)
                    .try_into()
                    .map_err(|_| RuntimeError::OutOfBoundsInteger)?;
                Ok(Self::Float(lhs.powi(rhs)))
            }
            _ => Err(RuntimeError::TypeError),
        }
    }
}

cmp_impl!(lt le gt ge);
