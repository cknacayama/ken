use std::str::Chars;

use thiserror::Error;

use crate::span::{Span, Spand};
use crate::token::{Token, TokenKind};

#[derive(Error, Debug, Clone, Copy)]
pub enum LexErrorKind {
    #[error("invalid character {0}")]
    InvalidChar(char),
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,

    /// start byte position of current token
    byte_start: u32,

    /// byte position of cursor
    byte: u32,
}

const EOF: char = '\0';

pub type LexError = Spand<LexErrorKind>;
pub type LexResult<T> = Result<T, LexError>;

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        assert!(u32::try_from(input.len()).is_ok());

        Self {
            input,
            chars: input.chars(),
            byte_start: 0,
            byte: 0,
        }
    }

    pub fn lex_all(self) -> LexResult<Vec<Token<'a>>> {
        self.collect()
    }

    fn first(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF)
    }

    fn second(&self) -> char {
        let mut iter = self.chars.clone();
        iter.next();
        iter.next().unwrap_or(EOF)
    }

    fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    fn bump(&mut self) -> Option<char> {
        #[allow(clippy::cast_possible_truncation)]
        self.chars
            .next()
            .inspect(|c| self.byte += c.len_utf8() as u32)
    }

    fn eat_while(&mut self, pred: impl Fn(char) -> bool) {
        while !self.is_eof() && pred(self.first()) {
            self.bump();
        }
    }

    const fn make_span(&self) -> Span {
        Span::new(self.byte_start, self.byte)
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() {
            match self.first() {
                '/' if self.second() == '/' => {
                    self.bump();
                    self.bump();
                    self.eat_while(|c| c != '\n');
                }
                c if c.is_ascii_whitespace() => {
                    self.bump();
                }
                _ => break,
            }
        }
    }

    fn view(&self) -> &'a str {
        &self.input[self.byte_start as usize..self.byte as usize]
    }

    fn number(&mut self) -> Token<'a> {
        self.eat_while(|c| c.is_ascii_digit());

        if self.first() == '.' && self.second().is_ascii_digit() {
            self.bump();
            self.bump();
            self.eat_while(|c| c.is_ascii_digit());
        }

        let s = self.view();

        Token::new(TokenKind::Number(s), self.make_span())
    }

    pub fn next_token(&mut self) -> Option<LexResult<Token<'a>>> {
        macro_rules! token {
            ($name:ident) => {
                Some(Ok(Token::new(TokenKind::$name, self.make_span())))
            };
            ($e:expr) => {
                Some(Ok($e))
            };
        }

        self.skip_whitespace();

        self.byte_start = self.byte;
        let c = self.bump()?;

        match c {
            '(' => token!(LParen),
            ')' => token!(RParen),
            '+' => token!(Plus),
            '-' => token!(Minus),
            '*' => token!(Star),
            '/' => token!(Slash),
            '^' => token!(Caret),
            '0'..='9' => token!(self.number()),

            _ => Some(Err(LexError::new(
                LexErrorKind::InvalidChar(c),
                self.make_span(),
            ))),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<Token<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
