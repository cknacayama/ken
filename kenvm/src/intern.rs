use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

use crate::obj::StrRef;
use crate::ty::Ty;

#[derive(Debug)]
pub struct Interned<T: ?Sized>(Rc<T>);

impl<T: ?Sized> Clone for Interned<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: ?Sized> Interned<T> {
    pub const fn new(s: Rc<T>) -> Self {
        Self(s)
    }

    #[must_use]
    pub fn as_ptr(&self) -> *const T {
        Rc::as_ptr(&self.0)
    }

    #[must_use]
    pub fn take(self) -> Rc<T> {
        self.0
    }

    #[must_use]
    pub const fn as_rc(&self) -> &Rc<T> {
        &self.0
    }
}

impl<T: ?Sized> AsRef<T> for Interned<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T: ?Sized> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::addr_eq(self.as_ptr(), other.as_ptr())
    }
}

impl<T: ?Sized> Eq for Interned<T> {}

impl<T: ?Sized> Hash for Interned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.as_ptr(), state);
    }
}

impl<T: ?Sized + Display> Display for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

pub trait Intern<T> {
    fn intern(&mut self, value: T) -> Interned<T>;
}

pub trait InternRef<'a, T: ?Sized> {
    fn intern_ref(&mut self, value: &'a T) -> Interned<T>;
}

impl<T, S: std::hash::BuildHasher> Intern<T> for HashSet<Rc<T>, S>
where
    T: Hash + Eq,
{
    fn intern(&mut self, value: T) -> Interned<T> {
        if let Some(value) = self.get(&value) {
            Interned::new(value.clone())
        } else {
            let value = Rc::new(value);
            self.insert(value.clone());
            Interned::new(value)
        }
    }
}
impl<S: std::hash::BuildHasher> Intern<Ty> for HashMap<StrRef, Rc<Ty>, S> {
    fn intern(&mut self, value: Ty) -> Interned<Ty> {
        let ty = self
            .entry(value.name().clone())
            .or_insert_with(|| Rc::new(value))
            .clone();
        Interned::new(ty)
    }
}

impl<'a, T, S: std::hash::BuildHasher> InternRef<'a, T> for HashSet<Rc<T>, S>
where
    T: 'a + ?Sized + Hash + Eq,
    Rc<T>: From<&'a T>,
{
    fn intern_ref(&mut self, value: &'a T) -> Interned<T> {
        if let Some(value) = self.get(value) {
            Interned::new(value.clone())
        } else {
            let value = Rc::from(value);
            self.insert(value.clone());
            Interned::new(value)
        }
    }
}
