use std::borrow::Cow;

use kenspan::{Span, Spand};

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

    #[must_use]
    pub const fn is_nonfix(&self) -> bool {
        matches!(self, Self::Infix)
    }

    #[must_use]
    pub const fn is_left(&self) -> bool {
        matches!(self, Self::InfixLeft)
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
    Rem,
    Pow,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    Assign,
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
            Self::Assign => (1, Fixity::Infix),
            Self::Eq | Self::Ne | Self::Gt | Self::Ge | Self::Lt | Self::Le => (2, Fixity::Infix),
            Self::Add | Self::Sub => (3, Fixity::InfixLeft),
            Self::Mul | Self::Div | Self::Rem => (4, Fixity::InfixLeft),
            Self::Pow => (5, Fixity::InfixRight),
        }
    }

    fn from_token(kind: TokenKind<'_>) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Add),
            TokenKind::Minus => Some(Self::Sub),
            TokenKind::Star => Some(Self::Mul),
            TokenKind::Slash => Some(Self::Div),
            TokenKind::Percent => Some(Self::Rem),
            TokenKind::Caret => Some(Self::Pow),
            TokenKind::Eq => Some(Self::Assign),
            TokenKind::EqEq => Some(Self::Eq),
            TokenKind::BangEq => Some(Self::Ne),
            TokenKind::Greater => Some(Self::Gt),
            TokenKind::GreaterEq => Some(Self::Ge),
            TokenKind::Less => Some(Self::Lt),
            TokenKind::LessEq => Some(Self::Le),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Box<[Stmt<'a>]>,
    pub span:  Span,
}

#[derive(Debug)]
pub struct Fn<'a> {
    pub name:   &'a str,
    pub params: Box<[&'a str]>,
    pub body:   Block<'a>,
}

#[derive(Debug)]
pub enum ItemKind<'a> {
    Fn(Fn<'a>),
    Let(Local<'a>),
}

#[derive(Debug)]
pub struct Local<'a> {
    pub name: &'a str,
    pub bind: Option<Expr<'a>>,
}

#[derive(Debug)]
pub enum StmtKind<'a> {
    Item(Item<'a>),
    Expr(Expr<'a>),
    Semi(Expr<'a>),
    Empty,
}

impl<'a> StmtKind<'a> {
    #[must_use]
    pub const fn is_last(&self) -> bool {
        matches!(self, Self::Expr(_) | Self::Empty)
    }
}

#[derive(Debug)]
pub struct TableEntry<'a> {
    pub key:   Expr<'a>,
    pub value: Expr<'a>,
}

#[derive(Debug)]
pub enum ExprKind<'a> {
    Unit,
    Ident(&'a str),
    Bool(bool),
    Float(f64),
    Integer(u32),
    String(Cow<'a, str>),

    Table(Box<[TableEntry<'a>]>),

    Block(Block<'a>),

    While {
        cond: Box<Expr<'a>>,
        body: Block<'a>,
    },

    List {
        tuple: bool,
        items: Box<[Expr<'a>]>,
    },

    If {
        cond: Box<Expr<'a>>,
        then: Block<'a>,
        els:  Block<'a>,
    },

    Call {
        callee: Box<Expr<'a>>,
        args:   Box<[Expr<'a>]>,
    },

    Idx {
        expr: Box<Expr<'a>>,
        idx:  Box<Expr<'a>>,
    },

    Field {
        expr:  Box<Expr<'a>>,
        field: &'a str,
    },

    Prefix {
        op:   PrefixOp,
        expr: Box<Expr<'a>>,
    },

    Infix {
        op:  InfixOp,
        lhs: Box<Expr<'a>>,
        rhs: Box<Expr<'a>>,
    },

    Lambda {
        params: Box<[&'a str]>,
        expr:   Box<Expr<'a>>,
    },
}

pub type Expr<'a> = Spand<ExprKind<'a>>;
pub type Stmt<'a> = Spand<StmtKind<'a>>;
pub type Item<'a> = Spand<ItemKind<'a>>;
