use std::fmt::Display;

use kenspan::Spand;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Number(&'a str),
    Ident(&'a str),

    Plus,
    Minus,
    Star,
    Slash,
    Caret,

    LParen,
    RParen,
    LBrace,
    RBrace,

    KwFn,
    KwRet,
    KwIf,
    KwElse,
}

pub type Token<'a> = Spand<TokenKind<'a>>;

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(x) | Self::Ident(x) => write!(f, "{x}"),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Caret => write!(f, "^"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),

            Self::KwFn => write!(f, "fn"),
            Self::KwRet => write!(f, "ret"),
            Self::KwIf => write!(f, "if"),
            Self::KwElse => write!(f, "else"),
        }
    }
}
