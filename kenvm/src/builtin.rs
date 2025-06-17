#![allow(
    clippy::unnecessary_wraps,
    reason = "builtin functions must have the same signature"
)]

use std::fmt::Display;

use crate::obj::{MutObj, Obj};
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    name: &'static str,
    ptr:  fn(&[Value]) -> RuntimeResult<Value>,
}

macro_rules! core_bultins {
    [] => {
        []
    };
    [$($name:ident),+ $(,)?] => {
        [$(Builtin::$name()),+]
    };
}

impl Builtin {
    pub const fn new(name: &'static str, ptr: fn(&[Value]) -> RuntimeResult<Value>) -> Self {
        Self { name, ptr }
    }

    #[must_use]
    pub fn core_builtins() -> impl ExactSizeIterator<Item = Self> {
        core_bultins![print, println, pow, push, len].into_iter()
    }

    pub const fn print() -> Self {
        Self::new("print", builtin_print)
    }

    pub const fn println() -> Self {
        Self::new("println", builtin_println)
    }

    pub const fn pow() -> Self {
        Self::new("pow", builtin_pow)
    }

    pub const fn push() -> Self {
        Self::new("push", builtin_push)
    }

    pub const fn len() -> Self {
        Self::new("len", builtin_len)
    }

    #[must_use]
    pub const fn name(&self) -> &'static str {
        self.name
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Builtin {}

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

fn builtin_println(args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        println!("{arg}");
    }
    Ok(Value::Unit)
}

fn get_arg(args: &[Value], pos: usize) -> RuntimeResult<&Value> {
    args.get(pos).ok_or(RuntimeError::NotEnoughArgs)
}

fn builtin_pow(args: &[Value]) -> RuntimeResult<Value> {
    let lhs = get_arg(args, 0)?;
    let rhs = get_arg(args, 1)?;
    let x = match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => lhs.powf(*rhs),
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Float(x))
}

fn builtin_push(args: &[Value]) -> RuntimeResult<Value> {
    let mut list = get_arg(args, 0)?.as_mut_obj()?.as_list_mut()?;
    let item = get_arg(args, 1)?;
    list.push(item.clone());
    Ok(Value::Unit)
}

fn builtin_len(args: &[Value]) -> RuntimeResult<Value> {
    let value = get_arg(args, 0)?;
    match value {
        Value::MutObj(obj) => {
            let obj = obj.borrow()?;
            match &*obj {
                MutObj::List(values) => Ok(Value::Int(values.len() as i64)),
                MutObj::Tuple(values) => Ok(Value::Int(values.len() as i64)),
                _ => Err(RuntimeError::TypeError),
            }
        }
        Value::Obj(obj) => match obj.as_ref() {
            Obj::String(string) => Ok(Value::Int(string.len() as i64)),
            _ => Err(RuntimeError::TypeError),
        },
        _ => Err(RuntimeError::TypeError),
    }
}
