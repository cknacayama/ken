use std::fmt::Display;

use kenspan::Spand;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Float(&'a str),
    Integer(&'a str),
    Ident(&'a str),
    String(&'a str),

    Dot,
    Comma,
    Colon,
    Semicolon,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,

    Bang,

    Eq,

    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    CaretEq,

    EqEq,
    BangEq,
    GreaterEq,
    LessEq,
    Greater,
    Less,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    KwTrue,
    KwFalse,

    KwStruct,
    KwEnum,
    KwFn,
    KwWhile,
    KwIf,
    KwElse,
    KwLet,
}

pub type Token<'a> = Spand<TokenKind<'a>>;

impl<'a> TokenKind<'a> {
    #[must_use]
    pub fn try_kw(s: &'a str) -> Self {
        match s {
            "true" => TokenKind::KwTrue,
            "false" => TokenKind::KwFalse,
            "struct" => TokenKind::KwStruct,
            "enum" => TokenKind::KwEnum,
            "fn" => TokenKind::KwFn,
            "while" => TokenKind::KwWhile,
            "if" => TokenKind::KwIf,
            "else" => TokenKind::KwElse,
            "let" => TokenKind::KwLet,
            _ => TokenKind::Ident(s),
        }
    }

    #[must_use]
    pub const fn can_start_item(&self) -> bool {
        matches!(
            self,
            Self::KwFn | Self::KwLet | Self::KwStruct | Self::KwEnum
        )
    }

    #[must_use]
    pub const fn can_recover(&self) -> bool {
        self.can_start_item()
    }
}

impl Display for TokenKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) | Self::Integer(x) | Self::Ident(x) | Self::String(x) => {
                write!(f, "{x}")
            }

            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Semicolon => write!(f, ";"),

            Self::Bang => write!(f, "!"),
            Self::Eq => write!(f, "="),

            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::StarEq => write!(f, "*="),
            Self::SlashEq => write!(f, "/="),
            Self::PercentEq => write!(f, "%="),
            Self::CaretEq => write!(f, "^="),

            Self::EqEq => write!(f, "=="),
            Self::BangEq => write!(f, "!="),
            Self::Less => write!(f, "<"),
            Self::LessEq => write!(f, "<="),
            Self::Greater => write!(f, ">"),
            Self::GreaterEq => write!(f, ">="),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Caret => write!(f, "^"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),

            Self::KwStruct => write!(f, "struct"),
            Self::KwEnum => write!(f, "enum"),
            Self::KwTrue => write!(f, "true"),
            Self::KwFalse => write!(f, "false"),
            Self::KwFn => write!(f, "fn"),
            Self::KwWhile => write!(f, "while"),
            Self::KwIf => write!(f, "if"),
            Self::KwElse => write!(f, "else"),
            Self::KwLet => write!(f, "let"),
        }
    }
}
