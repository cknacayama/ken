use kenspan::Spand;

use crate::token::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Prefix,
    Infix,
    InfixLeft,
    InfixRight,
    Postfix,
}

impl Fixity {
    #[must_use]
    pub const fn is_right(self) -> bool {
        matches!(self, Self::InfixRight)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OperatorData<T: Operator> {
    op:   T,
    prec: u8,
    fix:  Fixity,
}

impl<T: Operator> OperatorData<T> {
    pub const fn new(op: T, prec: u8, fix: Fixity) -> Self {
        Self { op, prec, fix }
    }

    pub const fn op(self) -> T {
        self.op
    }

    pub const fn prec(self) -> u8 {
        self.prec
    }

    pub const fn fix(self) -> Fixity {
        self.fix
    }
}

pub trait Operator: Copy {
    fn as_operator(self) -> (u8, Fixity);
    fn from_token(kind: TokenKind<'_>) -> Option<Self>;

    fn precedence(self) -> u8 {
        let (prec, _) = self.as_operator();
        prec
    }

    fn fixity(self) -> Fixity {
        let (_, fix) = self.as_operator();
        fix
    }

    fn as_data(self) -> OperatorData<Self> {
        let (prec, fix) = self.as_operator();
        OperatorData::new(self, prec, fix)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Operator for PrefixOp {
    fn as_operator(self) -> (u8, Fixity) {
        match self {
            Self::Neg => (9, Fixity::Prefix),
        }
    }

    fn from_token(kind: TokenKind<'_>) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(Self::Neg),
            _ => None,
        }
    }
}

impl Operator for InfixOp {
    fn as_operator(self) -> (u8, Fixity) {
        match self {
            Self::Add | Self::Sub => (3, Fixity::InfixLeft),
            Self::Mul | Self::Div => (4, Fixity::InfixLeft),
            Self::Pow => (5, Fixity::InfixRight),
        }
    }

    fn from_token(kind: TokenKind<'_>) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Minus => Some(Self::Sub),
            TokenKind::Star => Some(Self::Mul),
            TokenKind::Slash => Some(Self::Div),
            TokenKind::Caret => Some(Self::Pow),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(f64),

    Prefix {
        op:   PrefixOp,
        expr: Box<Expr>,
    },

    Infix {
        op:  InfixOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

pub type Expr = Spand<ExprKind>;
