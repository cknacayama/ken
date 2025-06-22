use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::obj::{MutObj, Obj};
use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TyId(usize);

impl TyId {
    pub const UNIT: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const INT: Self = Self(2);
    pub const FLOAT: Self = Self(3);
    pub const FUNCTION: Self = Self(4);
    pub const BUILTIN: Self = Self(5);
    pub const STRING: Self = Self(6);
    pub const TYPE: Self = Self(7);
    pub const LIST: Self = Self(8);
    pub const TUPLE: Self = Self(9);
    pub const TABLE: Self = Self(10);
    pub const OBJ: Self = Self(11);

    pub(crate) fn default_data(self) -> Ty {
        match self {
            Self::UNIT => Ty::new(self, Rc::from("unit")),
            Self::BOOL => Ty::new(self, Rc::from("bool")),
            Self::INT => Ty::new(self, Rc::from("int")),
            Self::FLOAT => Ty::new(self, Rc::from("float")),
            Self::FUNCTION => Ty::new(self, Rc::from("function")),
            Self::BUILTIN => Ty::new(self, Rc::from("builtin")),
            Self::STRING => {
                let mut ty = Ty::new(self, Rc::from("string"));
                ty.add_builtin(Builtin::len());
                ty
            }
            Self::TYPE => Ty::new(self, Rc::from("type")),
            Self::LIST => {
                let mut ty = Ty::new(self, Rc::from("list"));
                ty.add_builtin(Builtin::push());
                ty.add_builtin(Builtin::len());
                ty
            }
            Self::TUPLE => {
                let mut ty = Ty::new(self, Rc::from("tuple"));
                ty.add_builtin(Builtin::len());
                ty
            }
            Self::TABLE => {
                let mut ty = Ty::new(self, Rc::from("table"));
                ty.add_builtin(Builtin::len());
                ty
            }
            Self::OBJ => Ty::new(self, Rc::from("obj")),
            _ => todo!(),
        }
    }

    #[must_use]
    pub const fn get(self) -> usize {
        self.0
    }

    #[must_use]
    pub(crate) const fn new(id: usize) -> Self {
        Self(id)
    }
}

pub trait Typed {
    fn ty_id(&self) -> TyId;
}

impl Typed for Value {
    fn ty_id(&self) -> TyId {
        match self {
            Self::Unit => TyId::UNIT,
            Self::Bool(_) => TyId::BOOL,
            Self::Int(_) => TyId::INT,
            Self::Float(_) => TyId::FLOAT,
            Self::MutObj(obj) => obj.borrow().map(|obj| obj.ty_id()).unwrap_or(TyId::OBJ),
            Self::Obj(obj) => obj.ty_id(),
        }
    }
}

impl Typed for Obj {
    fn ty_id(&self) -> TyId {
        match self {
            Self::Function(_) => TyId::FUNCTION,
            Self::Builtin(_) => TyId::BUILTIN,
            Self::String(_) => TyId::STRING,
            Self::Type(_) => TyId::TYPE,
        }
    }
}

impl Typed for MutObj {
    fn ty_id(&self) -> TyId {
        match self {
            Self::List(_) => TyId::LIST,
            Self::Tuple(_) => TyId::TUPLE,
            Self::Table(_) => TyId::TABLE,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    id:      TyId,
    name:    Rc<str>,
    fields:  HashMap<Rc<str>, Value>,
    methods: HashMap<Rc<str>, Rc<Obj>>,
}

impl Ty {
    #[must_use]
    pub fn new(id: TyId, name: Rc<str>) -> Self {
        Self {
            id,
            name,
            fields: HashMap::default(),
            methods: HashMap::default(),
        }
    }

    fn add_builtin(&mut self, method: Builtin) {
        let name = Rc::from(method.name());
        let method = Rc::new(Obj::Builtin(method));
        self.methods.insert(name, method);
    }

    #[must_use]
    pub fn get_method(&self, name: &str) -> Option<&Rc<Obj>> {
        self.methods.get(name)
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}
