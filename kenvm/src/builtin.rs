#![allow(
    clippy::unnecessary_wraps,
    reason = "builtin functions must have the same signature"
)]

use std::fmt::Display;

use crate::obj::{MutObj, Obj};
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

pub type BuiltinFn = fn(args: &[Value]) -> RuntimeResult<Value>;

#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    name: &'static str,
    ptr:  BuiltinFn,
}

macro_rules! core_bultins {
    [] => {
        []
    };
    [$($name:ident),+ $(,)?] => {
        [$(Builtin::$name()),+]
    };
}

macro_rules! impl_core {
    [$($name:ident),* $(,)?] => {
        impl Builtin {
            const CORE_LEN: usize = core_bultins![$($name),*].len();
            const CORE: [Builtin; Self::CORE_LEN] = core_bultins![$($name),*];

            #[inline]
            #[must_use]
            pub const fn core_builtins() -> &'static [Builtin] {
                &Self::CORE
            }

            $(
                #[must_use]
                pub const fn $name() -> Builtin {
                    Self::new(stringify!($name), $name)
                }
            )*
        }
    };
}

impl_core![print, println, pow, push, len];

impl Builtin {
    pub const fn new(name: &'static str, ptr: fn(&[Value]) -> RuntimeResult<Value>) -> Self {
        Self { name, ptr }
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

fn print(args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        print!("{arg}");
    }
    Ok(Value::Unit)
}

fn println(args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        println!("{arg}");
    }
    Ok(Value::Unit)
}

fn get_arg(args: &[Value], pos: usize) -> RuntimeResult<&Value> {
    args.get(pos).ok_or(RuntimeError::NotEnoughArgs)
}

fn pow(args: &[Value]) -> RuntimeResult<Value> {
    let lhs = get_arg(args, 0)?;
    let rhs = get_arg(args, 1)?;
    let x = match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => lhs.powf(*rhs),
        _ => return Err(RuntimeError::TypeError),
    };
    Ok(Value::Float(x))
}

fn push(args: &[Value]) -> RuntimeResult<Value> {
    let mut list = get_arg(args, 0)?.as_mut_obj()?.as_list_mut()?;
    let item = get_arg(args, 1)?;
    list.push(item.clone());
    Ok(Value::Unit)
}

fn len(args: &[Value]) -> RuntimeResult<Value> {
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
