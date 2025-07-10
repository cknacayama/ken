use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::hash::{HashValue, Table};
use crate::intern::{Intern, InternRef, Interned};
use crate::obj::{MutObj, Obj, ObjRef, StrRef};
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Ty {
    name:    StrRef,
    ctors:   Table<ObjRef>,
    methods: HashMap<StrRef, ObjRef>,
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for Ty {}

impl Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Ty {
    #[must_use]
    pub fn new(name: StrRef) -> Self {
        Self {
            name,
            ctors: Table::default(),
            methods: HashMap::default(),
        }
    }

    pub fn add_method(&mut self, name: StrRef, method: ObjRef) {
        self.methods.insert(name, method);
    }

    pub fn add_ctor(&mut self, key: HashValue, ctor: ObjRef) {
        self.ctors.insert(key, ctor);
    }

    fn add_builtin<'a>(&mut self, set: &mut impl InternRef<'a, str>, method: Builtin) {
        let name = set.intern_ref(method.name());
        let method = ObjRef::new(Obj::Builtin(method));
        self.add_method(name, method);
    }

    fn add_builtin_ctor(&mut self, key: HashValue, ctor: Builtin) {
        let ctor = ObjRef::new(Obj::Builtin(ctor));
        self.add_ctor(key, ctor);
    }

    #[must_use]
    pub fn get_method(&self, name: &StrRef) -> Option<&ObjRef> {
        self.methods.get(name)
    }

    #[must_use]
    pub fn get_ctor(&self, key: &HashValue) -> Option<&ObjRef> {
        self.ctors.get(key)
    }

    #[must_use]
    pub const fn name(&self) -> &StrRef {
        &self.name
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)
    }
}

pub type TyRef = Interned<Ty>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ctor {
    ty:     StrRef,
    name:   StrRef,
    fields: Box<[StrRef]>,
}

impl Ctor {
    #[must_use]
    pub const fn new(ty: StrRef, name: StrRef, fields: Box<[StrRef]>) -> Self {
        Self { ty, name, fields }
    }

    #[must_use]
    pub const fn name(&self) -> &StrRef {
        &self.name
    }

    #[must_use]
    pub fn fields(&self) -> &[StrRef] {
        &self.fields
    }

    #[must_use]
    pub const fn ty(&self) -> &StrRef {
        &self.ty
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    ty:     TyRef,
    ctor:   StrRef,
    fields: Table<Value>,
}

impl Instance {
    #[must_use] pub const fn new(ty: TyRef, ctor: StrRef, fields: Table<Value>) -> Self {
        Self { ty, ctor, fields }
    }

    #[must_use] pub const fn fields(&self) -> &Table<Value> {
        &self.fields
    }

    pub const fn fields_mut(&mut self) -> &mut Table<Value> {
        &mut self.fields
    }

    #[must_use] pub const fn ty(&self) -> &TyRef {
        &self.ty
    }

    #[must_use] pub const fn ctor(&self) -> &StrRef {
        &self.ctor
    }
}

#[derive(Debug, Clone)]
pub struct TyCtx {
    unit_:   Rc<Ty>,
    int_:    Rc<Ty>,
    bool_:   Rc<Ty>,
    float_:  Rc<Ty>,
    fn_:     Rc<Ty>,
    bultin_: Rc<Ty>,
    str_:    Rc<Ty>,
    ty_:     Rc<Ty>,
    list_:   Rc<Ty>,
    tuple_:  Rc<Ty>,
    table_:  Rc<Ty>,

    types: HashMap<StrRef, Rc<Ty>>,
}

impl TyCtx {
    pub fn new<'a>(set: &mut impl InternRef<'a, str>) -> Self {
        let unit_ = Ty::new(set.intern_ref("unit"));
        let int_ = Ty::new(set.intern_ref("int"));
        let bool_ = Ty::new(set.intern_ref("bool"));
        let float_ = Ty::new(set.intern_ref("float"));
        let fn_ = Ty::new(set.intern_ref("fn"));
        let bultin_ = Ty::new(set.intern_ref("bultin"));
        let mut str_ = Ty::new(set.intern_ref("str"));
        let mut ty_ = Ty::new(set.intern_ref("type"));
        let mut list_ = Ty::new(set.intern_ref("list"));
        let mut tuple_ = Ty::new(set.intern_ref("tuple"));
        let mut table_ = Ty::new(set.intern_ref("table"));

        str_.add_builtin(set, Builtin::len());
        list_.add_builtin(set, Builtin::len());
        list_.add_builtin(set, Builtin::push());
        tuple_.add_builtin(set, Builtin::len());
        table_.add_builtin(set, Builtin::len());

        ty_.add_builtin_ctor(HashValue::Int(1), Builtin::typ_());
        table_.add_builtin_ctor(HashValue::Int(1), Builtin::table());

        #[allow(clippy::mutable_key_type)]
        let mut types = HashMap::new();
        let unit_ = types.intern(unit_).take();
        let int_ = types.intern(int_).take();
        let bool_ = types.intern(bool_).take();
        let float_ = types.intern(float_).take();
        let fn_ = types.intern(fn_).take();
        let bultin_ = types.intern(bultin_).take();
        let str_ = types.intern(str_).take();
        let ty_ = types.intern(ty_).take();
        let list_ = types.intern(list_).take();
        let tuple_ = types.intern(tuple_).take();
        let table_ = types.intern(table_).take();

        Self {
            unit_,
            int_,
            bool_,
            float_,
            fn_,
            bultin_,
            str_,
            ty_,
            list_,
            tuple_,
            table_,
            types,
        }
    }

    #[must_use]
    pub fn builtin_types(&self) -> [TyRef; 11] {
        [
            TyRef::new(self.unit_.clone()),
            TyRef::new(self.int_.clone()),
            TyRef::new(self.bool_.clone()),
            TyRef::new(self.float_.clone()),
            TyRef::new(self.fn_.clone()),
            TyRef::new(self.bultin_.clone()),
            TyRef::new(self.str_.clone()),
            TyRef::new(self.ty_.clone()),
            TyRef::new(self.list_.clone()),
            TyRef::new(self.tuple_.clone()),
            TyRef::new(self.table_.clone()),
        ]
    }

    #[must_use]
    pub fn type_of_value(&self, value: &Value) -> TyRef {
        match value {
            Value::Unit => TyRef::new(self.unit_.clone()),
            Value::Bool(_) => TyRef::new(self.bool_.clone()),
            Value::Int(_) => TyRef::new(self.int_.clone()),
            Value::Float(_) => TyRef::new(self.float_.clone()),
            Value::MutObj(mut_obj_ref) => self.type_of_mut_obj(&mut_obj_ref.borrow().unwrap()),
            Value::Obj(obj_ref) => self.type_of_obj(obj_ref),
        }
    }

    #[must_use]
    pub fn type_of_obj(&self, obj: &Obj) -> TyRef {
        match obj {
            Obj::Function(_) | Obj::Ctor(_) => TyRef::new(self.fn_.clone()),
            Obj::Builtin(_) => TyRef::new(self.bultin_.clone()),
            Obj::Str(_) => TyRef::new(self.str_.clone()),
            Obj::Ty(_) => TyRef::new(self.ty_.clone()),
        }
    }

    #[must_use]
    pub fn type_of_mut_obj(&self, obj: &MutObj) -> TyRef {
        match obj {
            MutObj::List(_) => TyRef::new(self.list_.clone()),
            MutObj::Tuple(_) => TyRef::new(self.tuple_.clone()),
            MutObj::Table(_) => TyRef::new(self.table_.clone()),
            MutObj::Instance(obj) => obj.ty.clone(),
        }
    }

    pub fn intern(&mut self, ty: Ty) -> TyRef {
        self.types.intern(ty)
    }

    pub fn get(&self, ty_name: &StrRef) -> Option<TyRef> {
        self.types.get(ty_name).cloned().map(TyRef::new)
    }
}
