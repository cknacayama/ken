#![allow(
    clippy::unnecessary_wraps,
    reason = "builtin functions must have the same signature"
)]

use std::fmt::Display;

use crate::value::{Convert, Value};
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Builtin {
    name: &'static str,
    ptr:  fn(&[Value]) -> RuntimeResult<Value>,
}

impl Builtin {
    pub const fn new(name: &'static str, ptr: fn(&[Value]) -> RuntimeResult<Value>) -> Self {
        Self { name, ptr }
    }

    pub const fn print() -> Self {
        Self::new("print", builtin_print)
    }

    pub const fn pow() -> Self {
        Self::new("pow", builtin_pow)
    }
}

impl std::ops::Deref for Builtin {
    type Target = fn(&[Value]) -> RuntimeResult<Value>;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin '{}' at {:?}>", self.name, self.ptr)
    }
}

fn builtin_print(args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        print!("{arg}");
    }
    Ok(Value::Unit)
}

fn get_arg<T: Convert>(args: &[Value], pos: usize) -> RuntimeResult<T> {
    args.get(pos)
        .cloned()
        .ok_or(RuntimeError::NotEnoughArgs)?
        .try_into()
}

fn builtin_pow(args: &[Value]) -> RuntimeResult<Value> {
    let lhs = get_arg(args, 0)?;
    let rhs = get_arg(args, 1)?;
    let x = f64::powf(lhs, rhs);
    Ok(Value::Number(x))
}
