#![allow(
    clippy::unnecessary_wraps,
    reason = "builtin functions must have the same signature"
)]

use std::fmt::Display;
use std::rc::Rc;

use crate::obj::{MutObj, Obj};
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult, Vm};

pub type BuiltinFn = fn(vm: &Vm, args: &[Value]) -> RuntimeResult<Value>;

fn print(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        print!("{arg}");
    }
    Ok(Value::Unit)
}

fn println(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    for arg in args {
        println!("{arg}");
    }
    Ok(Value::Unit)
}

fn get_arg(args: &[Value], pos: usize) -> RuntimeResult<&Value> {
    args.get(pos).ok_or(RuntimeError::NotEnoughArgs)
}

fn assert(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let cond = get_arg(args, 0)?.clone().try_into()?;
    if cond {
        Ok(Value::Unit)
    } else {
        Err(RuntimeError::AssertFailed)
    }
}

fn push(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let mut list = get_arg(args, 0)?.as_mut_obj()?.as_list_mut()?;
    let item = get_arg(args, 1)?;
    list.push(item.clone());
    Ok(Value::Unit)
}

fn len(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let value = get_arg(args, 0)?;
    match value {
        Value::MutObj(obj) => {
            let obj = obj.borrow()?;
            match &*obj {
                MutObj::List(values) => try_cast_int(values.len()),
                MutObj::Tuple(values) => try_cast_int(values.len()),
                MutObj::Table(values) => try_cast_int(values.len()),
            }
        }
        Value::Obj(obj) => match obj.as_ref() {
            Obj::String(string) => try_cast_int(string.len()),
            _ => Err(RuntimeError::TypeError),
        },
        _ => Err(RuntimeError::TypeError),
    }
}

fn disassemble(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let obj = get_arg(args, 0)?.as_obj()?;
    let ptr = Rc::as_ptr(obj);
    let f = get_arg(args, 0)?
        .as_obj()?
        .as_function()
        .ok_or(RuntimeError::TypeError)?;
    println!("<fn at {ptr:?}>:");
    if let Some(name) = f.name() {
        println!("  name: {name}");
    }
    println!("  arity: {}", f.arity());
    print!("{}", f.chunk());
    Ok(Value::Unit)
}

#[derive(Debug, Clone, Copy)]
pub struct Builtin {
    name: &'static str,
    ptr:  BuiltinFn,
}

impl Builtin {
    pub const fn new(name: &'static str, ptr: fn(&Vm, &[Value]) -> RuntimeResult<Value>) -> Self {
        Self { name, ptr }
    }

    #[must_use]
    pub const fn name(&self) -> &'static str {
        self.name
    }
}

fn try_cast_int<T>(from: T) -> RuntimeResult<Value>
where
    i64: TryFrom<T>,
{
    from.try_into()
        .map_err(|_| RuntimeError::OutOfBoundsInteger)
        .map(Value::Int)
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Builtin {}

impl std::ops::Deref for Builtin {
    type Target = BuiltinFn;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin '{}' at {:?}>", self.name, self.ptr)
    }
}

macro_rules! core_bultins {
    [$($name:ident),* $(,)?] => {
        [$(Builtin::$name()),*]
    };
}

macro_rules! impl_core {
    [$($name:ident),* $(,)?] => {
        impl Builtin {
            const CORE_LEN: usize = core_bultins![$($name),*].len();
            const CORE: [Builtin; Self::CORE_LEN] = core_bultins![$($name),*];

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

impl_core![print, println, push, len, disassemble, assert];
