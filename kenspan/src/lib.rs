use std::error::Error;
use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    lo: u32,
    hi: u32,
}

impl Span {
    #[must_use]
    #[inline]
    pub const fn new(lo: u32, hi: u32) -> Self {
        if hi < lo {
            Self { lo: hi, hi: lo }
        } else {
            Self { lo, hi }
        }
    }

    #[must_use]
    #[inline]
    pub const fn len(self) -> u32 {
        self.hi - self.lo
    }

    #[must_use]
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.len() == 0
    }

    #[must_use]
    #[inline]
    pub fn join(self, other: Self) -> Self {
        let lo = std::cmp::min(self.lo, other.lo);
        let hi = std::cmp::max(self.hi, other.hi);

        Self::new(lo, hi)
    }

    #[must_use]
    #[inline]
    pub const fn lo(self) -> u32 {
        self.lo
    }

    #[must_use]
    #[inline]
    pub const fn hi(self) -> u32 {
        self.hi
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.lo as usize..value.hi as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spand<T> {
    pub kind: T,
    pub span: Span,
}

impl<T: Display> Display for Spand<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.kind(), f)
    }
}

impl<T: Error> Error for Spand<T> {}

impl<T> Spand<T> {
    #[inline]
    pub const fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }

    #[inline]
    pub const fn kind(&self) -> &T {
        &self.kind
    }
}
