use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::bytecode::Chunk;
use crate::value::Value;

pub type MutObjRef = Rc<RefCell<MutObj>>;
pub type ObjRef = Rc<Obj>;

#[derive(Debug, PartialEq)]
pub enum Obj {
    Function(Function),
    Builtin(Builtin),
    String(Rc<str>),
}

#[derive(Debug, PartialEq)]
pub enum MutObj {
    List(Vec<Value>),
    Closure(Closure),
}

impl MutObj {
    #[must_use]
    pub const fn is_pretty(&self) -> bool {
        matches!(self, Self::List(_))
    }
}

impl Obj {
    #[must_use]
    pub const fn is_pretty(&self) -> bool {
        matches!(self, Self::String(_))
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    arity: u8,
    chunk: Chunk,
}

impl Function {
    #[must_use]
    pub const fn new(arity: u8, chunk: Chunk) -> Self {
        Self { arity, chunk }
    }

    #[must_use]
    pub const fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    #[must_use]
    pub const fn arity(&self) -> u8 {
        self.arity
    }
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    captures: Vec<Value>,
    function: Function,
}

fn print_list(values: &[Value], f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[")?;
    let mut first = true;
    for value in values {
        if first {
            first = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, "{value}")?;
    }
    write!(f, "]")
}

impl Display for MutObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::List(values) => print_list(values, f),
            Self::Closure(closure) => {
                write!(f, "fn{{ arity: {}, closed: ", closure.function.arity())?;
                print_list(&closure.captures, f)?;
                write!(f, " }}")
            }
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => write!(f, "fn{{ arity: {} }}", function.arity()),
            Self::Builtin(builtin) => write!(f, "builtin '{}'", builtin.name()),
            Self::String(string) => write!(f, "{string}"),
        }
    }
}
