use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{BuildHasher, RandomState};
use std::ops::{Deref, DerefMut};

use crate::obj::{MutObj, Obj, StrRef};
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashValue {
    Unit,
    Bool(bool),
    Int(i64),
    Str(StrRef),
    Slice(Box<[HashValue]>),
}

#[derive(Debug, Clone)]
pub struct Table<V, S = RandomState> {
    table: HashMap<HashValue, V, S>,
}

impl<V: PartialEq, S: BuildHasher> PartialEq for Table<V, S> {
    fn eq(&self, other: &Self) -> bool {
        self.table == other.table
    }
}

impl<V, S> Deref for Table<V, S> {
    type Target = HashMap<HashValue, V, S>;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl<V, S> DerefMut for Table<V, S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.table
    }
}

impl<V, S: Default> Default for Table<V, S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V, S: Default> Table<V, S> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            table: HashMap::default(),
        }
    }
}

impl<V, S> IntoIterator for Table<V, S> {
    type Item = (HashValue, V);
    type IntoIter = std::collections::hash_map::IntoIter<HashValue, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.table.into_iter()
    }
}

impl<'a, V, S> IntoIterator for &'a Table<V, S> {
    type Item = (&'a HashValue, &'a V);
    type IntoIter = std::collections::hash_map::Iter<'a, HashValue, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.table.iter()
    }
}

impl<V, S: Default + BuildHasher> FromIterator<(HashValue, V)> for Table<V, S> {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (HashValue, V)>,
    {
        Self {
            table: HashMap::from_iter(iter),
        }
    }
}

impl<V, S: BuildHasher> Table<V, S> {
    pub fn try_get(&self, k: &Value) -> RuntimeResult<&V> {
        let k = k.as_hash().ok_or(RuntimeError::NotHash)?;
        self.table.get(&k).ok_or(RuntimeError::NoValue)
    }

    pub fn try_insert(&mut self, k: &Value, v: V) -> RuntimeResult<Option<V>> {
        let k = k.as_hash().ok_or(RuntimeError::NotHash)?;
        Ok(self.table.insert(k, v))
    }

    pub fn try_get_mut(&mut self, k: &Value) -> RuntimeResult<&mut V> {
        let k = k.as_hash().ok_or(RuntimeError::NotHash)?;
        self.table.get_mut(&k).ok_or(RuntimeError::NoValue)
    }
}

impl Display for HashValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(x) => write!(f, "{x}"),
            Self::Str(s) => write!(f, "{s}"),
            Self::Slice(s) => {
                let mut first = true;
                write!(f, "[")?;
                for v in s {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{v}")?;
                }
                write!(f, "]")
            }
        }
    }
}

impl Value {
    #[must_use]
    pub fn as_hash(&self) -> Option<HashValue> {
        match self {
            Self::Unit => Some(HashValue::Unit),
            Self::Bool(b) => Some(HashValue::Bool(*b)),
            Self::Int(x) => Some(HashValue::Int(*x)),
            Self::Obj(obj) => match obj.as_ref() {
                Obj::Str(s) => Some(HashValue::Str(s.clone())),
                _ => None,
            },
            Self::MutObj(obj) => {
                let obj = obj.borrow().ok()?;
                match &*obj {
                    MutObj::List(values) => values
                        .iter()
                        .map(Self::as_hash)
                        .collect::<Option<_>>()
                        .map(HashValue::Slice),
                    MutObj::Tuple(values) => values
                        .iter()
                        .map(Self::as_hash)
                        .collect::<Option<_>>()
                        .map(HashValue::Slice),
                    MutObj::Table(_) => todo!(),
                    MutObj::Instance(_) => todo!(),
                }
            }
            Self::Float(_) => None,
        }
    }
}
