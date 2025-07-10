use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Display;
use std::ops::Deref;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::bytecode::Chunk;
use crate::hash::Table;
use crate::intern::Interned;
use crate::ty::{Ctor, Instance, TyRef};
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

pub type StrRef = Interned<str>;

impl StrRef {
    #[must_use]
    pub fn len(&self) -> usize {
        self.as_ref().len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.as_ref().is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjRef(Rc<Obj>);

impl ObjRef {
    #[must_use]
    pub fn new(obj: Obj) -> Self {
        Self(Rc::new(obj))
    }

    #[must_use]
    pub fn as_ptr(&self) -> *const Obj {
        Rc::as_ptr(&self.0)
    }
}

impl Deref for ObjRef {
    type Target = Obj;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<Obj> for ObjRef {
    fn as_ref(&self) -> &Obj {
        &self.0
    }
}

impl Display for ObjRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_ref(), f)
    }
}

impl<T> From<T> for ObjRef
where
    Obj: From<T>,
{
    fn from(value: T) -> Self {
        Self::new(Obj::from(value))
    }
}

#[derive(Debug, PartialEq)]
pub enum Obj {
    Function(Function),
    Builtin(Builtin),
    Str(StrRef),
    Ctor(Ctor),
    Ty(TyRef),
}

impl From<Function> for Obj {
    fn from(v: Function) -> Self {
        Self::Function(v)
    }
}

impl From<Builtin> for Obj {
    fn from(v: Builtin) -> Self {
        Self::Builtin(v)
    }
}

impl From<StrRef> for Obj {
    fn from(v: StrRef) -> Self {
        Self::Str(v)
    }
}

impl From<TyRef> for Obj {
    fn from(v: TyRef) -> Self {
        Self::Ty(v)
    }
}

impl From<Ctor> for Obj {
    fn from(v: Ctor) -> Self {
        Self::Ctor(v)
    }
}

#[derive(Debug, PartialEq)]
pub enum MutObj {
    List(Vec<Value>),
    Tuple(Box<[Value]>),
    Table(Table<Value>),
    Instance(Instance),
}

impl From<Instance> for MutObj {
    fn from(v: Instance) -> Self {
        Self::Instance(v)
    }
}

impl From<Table<Value>> for MutObj {
    fn from(v: Table<Value>) -> Self {
        Self::Table(v)
    }
}

impl From<Box<[Value]>> for MutObj {
    fn from(v: Box<[Value]>) -> Self {
        Self::Tuple(v)
    }
}

impl From<Vec<Value>> for MutObj {
    fn from(v: Vec<Value>) -> Self {
        Self::List(v)
    }
}

impl MutObj {
    #[must_use]
    const fn as_list(&self) -> Option<&[Value]> {
        if let Self::List(v) = self {
            Some(v.as_slice())
        } else {
            None
        }
    }

    #[must_use]
    const fn as_list_mut(&mut self) -> Option<&mut Vec<Value>> {
        if let Self::List(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    const fn as_tuple(&self) -> Option<&[Value]> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    const fn as_tuple_mut(&mut self) -> Option<&mut [Value]> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    const fn as_table(&self) -> Option<&Table<Value>> {
        match self {
            Self::Table(table) => Some(table),
            Self::Instance(instance) => Some(instance.fields()),
            _ => None,
        }
    }

    #[must_use]
    const fn as_table_mut(&mut self) -> Option<&mut Table<Value>> {
        match self {
            Self::Table(table) => Some(table),
            Self::Instance(instance) => Some(instance.fields_mut()),
            _ => None,
        }
    }
}

impl Obj {
    #[must_use]
    pub const fn is_pretty(&self) -> bool {
        matches!(self, Self::Str(_))
    }

    #[must_use]
    pub const fn as_string(&self) -> Option<&StrRef> {
        if let Self::Str(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_function(&self) -> Option<&Function> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_type(&self) -> Option<&TyRef> {
        if let Self::Ty(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn is_ty(&self) -> bool {
        matches!(self, Self::Ty(..))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MutObjRef(Rc<RefCell<MutObj>>);

impl MutObjRef {
    #[must_use]
    pub fn new(obj: MutObj) -> Self {
        Self(Rc::new(RefCell::new(obj)))
    }

    #[must_use]
    pub fn as_ptr(&self) -> *const RefCell<MutObj> {
        Rc::as_ptr(&self.0)
    }

    pub fn borrow(&self) -> RuntimeResult<Ref<'_, MutObj>> {
        self.0.try_borrow().map_err(|_| RuntimeError::BorrowError)
    }

    pub fn mut_borrow(&self) -> RuntimeResult<RefMut<'_, MutObj>> {
        self.0
            .try_borrow_mut()
            .map_err(|_| RuntimeError::BorrowError)
    }

    pub fn as_tuple(&self) -> RuntimeResult<Ref<'_, [Value]>> {
        let obj = self.borrow()?;
        Ref::filter_map(obj, MutObj::as_tuple).map_err(|_| RuntimeError::TypeError)
    }

    pub fn as_tuple_mut(&self) -> RuntimeResult<RefMut<'_, [Value]>> {
        let obj = self.mut_borrow()?;
        RefMut::filter_map(obj, MutObj::as_tuple_mut).map_err(|_| RuntimeError::TypeError)
    }

    pub fn as_list(&self) -> RuntimeResult<Ref<'_, [Value]>> {
        let obj = self.borrow()?;
        Ref::filter_map(obj, MutObj::as_list).map_err(|_| RuntimeError::TypeError)
    }

    pub fn as_list_mut(&self) -> RuntimeResult<RefMut<'_, Vec<Value>>> {
        let obj = self.mut_borrow()?;
        RefMut::filter_map(obj, MutObj::as_list_mut).map_err(|_| RuntimeError::TypeError)
    }

    pub fn as_table(&self) -> RuntimeResult<Ref<'_, Table<Value>>> {
        let obj = self.borrow()?;
        Ref::filter_map(obj, MutObj::as_table).map_err(|_| RuntimeError::TypeError)
    }

    pub fn as_table_mut(&self) -> RuntimeResult<RefMut<'_, Table<Value>>> {
        let obj = self.mut_borrow()?;
        RefMut::filter_map(obj, MutObj::as_table_mut).map_err(|_| RuntimeError::TypeError)
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name:  Option<StrRef>,
    arity: usize,
    chunk: Chunk,
}

impl Function {
    #[must_use]
    pub const fn new(name: Option<StrRef>, arity: usize, chunk: Chunk) -> Self {
        Self { name, arity, chunk }
    }

    #[must_use]
    pub const fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    #[must_use]
    pub const fn arity(&self) -> usize {
        self.arity
    }

    #[must_use]
    pub const fn name(&self) -> Option<&StrRef> {
        self.name.as_ref()
    }
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    captures: Vec<Value>,
    function: Function,
}

fn print_list<'a, T: Display + 'a>(
    values: impl IntoIterator<Item = &'a T>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let mut first = true;
    for value in values {
        if first {
            first = false;
        } else {
            write!(f, ", ")?;
        }
        write!(f, "{value}")?;
    }
    Ok(())
}

impl Display for MutObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::List(values) => {
                write!(f, "[")?;
                print_list(values, f)?;
                write!(f, "]")
            }
            Self::Tuple(values) => {
                write!(f, "(")?;
                print_list(values, f)?;
                write!(f, ")")
            }
            Self::Table(values) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in values {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")
            }
            Self::Instance(obj) => {
                write!(f, "{}", obj.ctor())?;
                if obj.fields().is_empty() {
                    return Ok(());
                }
                write!(f, " {{ ")?;
                let mut first = true;
                for (key, value) in obj.fields() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, " }}")
            }
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(function) => match function.name() {
                Some(name) => write!(f, "fn{{ name: {name}, arity: {} }}", function.arity()),
                None => write!(f, "fn{{ arity: {} }}", function.arity()),
            },
            Self::Builtin(builtin) => write!(f, "builtin '{}'", builtin.name()),
            Self::Ctor(ctor) => write!(f, "ctor '{}'", ctor.name()),
            Self::Str(string) => write!(f, "{string}"),
            Self::Ty(ty) => write!(f, "{ty}"),
        }
    }
}
