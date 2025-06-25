use std::collections::HashMap;
use std::fmt::Display;
use std::hash::{BuildHasher, RandomState};
use std::ops::{Deref, DerefMut};

use crate::obj::StrRef;
use crate::value::Value;
use crate::{RuntimeError, RuntimeResult};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashValue {
    Unit,
    Bool(bool),
    Int(i64),
    Str(StrRef),
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
        }
    }
}
