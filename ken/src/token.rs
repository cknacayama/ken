use crate::span::Spand;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Number(&'a str),

    Plus,
    Minus,
    Star,
    Slash,
    Caret,

    LParen,
    RParen,
}

pub type Token<'a> = Spand<TokenKind<'a>>;
