#![allow(
    clippy::unnecessary_wraps,
    reason = "builtin functions must have the same signature"
)]

use std::fmt::Display;

use crate::hash::{HashValue, Table};
use crate::obj::{MutObj, Obj, ObjRef};
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
                MutObj::List(values) => try_cast_int_value(values.len()),
                MutObj::Tuple(values) => try_cast_int_value(values.len()),
                MutObj::Table(values) => try_cast_int_value(values.len()),
                MutObj::Instance(_) => Err(RuntimeError::TypeError),
            }
        }
        Value::Obj(obj) => match obj.as_ref() {
            Obj::Str(string) => try_cast_int_value(string.len()),
            _ => Err(RuntimeError::TypeError),
        },
        _ => Err(RuntimeError::TypeError),
    }
}

fn disassemble(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let obj = get_arg(args, 0)?.as_obj()?;
    let ptr = obj.as_ptr();
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

fn typ_(vm: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let arg = get_arg(args, 0)?;
    let ty = vm.type_of_value(arg);
    Ok(Value::Obj(ObjRef::new(Obj::Ty(ty))))
}

#[allow(clippy::cast_possible_wrap)]
fn table(_: &Vm, args: &[Value]) -> RuntimeResult<Value> {
    let arg = get_arg(args, 0)?;
    match arg {
        Value::MutObj(obj) => match &*obj.borrow()? {
            MutObj::List(values) => {
                assert!(i64::try_from(values.len()).is_ok());
                let mut table = Table::default();
                for (i, value) in values.iter().enumerate() {
                    table.insert(HashValue::Int(i as i64), value.clone());
                }
                Ok(Value::from(MutObj::Table(table)))
            }
            MutObj::Tuple(values) => {
                assert!(i64::try_from(values.len()).is_ok());
                let mut table = Table::default();
                for (i, value) in values.iter().enumerate() {
                    table.insert(HashValue::Int(i as i64), value.clone());
                }
                Ok(Value::from(MutObj::Table(table)))
            }
            MutObj::Table(_) => Ok(arg.clone()),
            MutObj::Instance(obj) => Ok(Value::from(MutObj::Table(obj.fields().clone()))),
        },
        _ => Err(RuntimeError::TypeError),
    }
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

fn try_cast_int_value<T>(from: T) -> RuntimeResult<Value>
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

macro_rules! core_builtin {
    [$($name:ident),* $(,)?] => {
        [$(Builtin::$name()),*]
    };
}

macro_rules! builtin_name {
    ($name:ident) => {
        stringify!($name)
    };
    ($_:ident, $name:literal) => {
        $name
    };
}

macro_rules! impl_core {
    [$($name:ident $($method_name:literal)?),* $(,)?] => {
        impl Builtin {
            const CORE_LEN: usize = core_builtin![$($name),*].len();
            const CORE: [Builtin; Self::CORE_LEN] = core_builtin![$($name),*];

            #[must_use]
            pub const fn core_builtins() -> [Builtin; Self::CORE_LEN] {
                Self::CORE
            }

            $(
                #[must_use]
                pub const fn $name() -> Builtin {
                    Self::new(builtin_name!($name $(,$method_name)?), $name)
                }
            )*
        }
    };
}

impl_core![print, println, push, len, disassemble, assert, typ_ "typeof", table "to_table"];
