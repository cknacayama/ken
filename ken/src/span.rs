use std::str::Chars;

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    lo: u32,
    hi: u32,
}

impl Span {
    #[must_use]
    pub const fn new(lo: u32, hi: u32) -> Self {
        if hi < lo {
            Self { lo: hi, hi: lo }
        } else {
            Self { lo, hi }
        }
    }

    #[must_use]
    pub const fn len(self) -> u32 {
        self.hi - self.lo
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn join(self, other: Self) -> Self {
        let lo = std::cmp::min(self.lo, other.lo);
        let hi = std::cmp::max(self.hi, other.hi);

        Self::new(lo, hi)
    }

    #[must_use]
    pub const fn lo(self) -> u32 {
        self.lo
    }

    #[must_use]
    pub const fn hi(self) -> u32 {
        self.hi
    }

    #[must_use]
    pub fn decode(self, src: &str) -> Option<(Loc, Loc)> {
        let mut finder = LocIter::new(src);

        let lo = finder.nth(self.lo as usize)?;
        let hi = finder.nth((self.hi - self.lo - 1) as usize)?;

        Some((lo, hi))
    }
}

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq)]
#[error("{kind}")]
pub struct Spand<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Spand<T> {
    pub const fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    pub line:   u32,
    pub column: u32,
}

pub struct LocIter<'a> {
    curr: usize,
    next: usize,

    line:   u32,
    column: u32,

    chars: Chars<'a>,
}

impl<'a> LocIter<'a> {
    #[must_use]
    pub fn new(src: &'a str) -> Self {
        Self {
            curr:   0,
            next:   0,
            line:   1,
            column: 1,
            chars:  src.chars(),
        }
    }
}

impl Iterator for LocIter<'_> {
    type Item = Loc;

    fn next(&mut self) -> Option<Self::Item> {
        if self.curr > self.next {
            let c = self.chars.next()?;
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.next += c.len_utf8();
        }
        self.curr += 1;
        Some(Loc {
            line:   self.line,
            column: self.column,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.chars.as_str().len();
        let size = len + self.next - self.curr;
        (size, Some(size))
    }
}

impl ExactSizeIterator for LocIter<'_> {}
