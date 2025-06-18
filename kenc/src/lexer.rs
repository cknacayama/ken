use std::str::Chars;

use kenspan::{Span, Spand};
use thiserror::Error;

use crate::token::{Token, TokenKind};

#[derive(Error, Debug, Clone, Copy)]
pub enum LexErrorKind {
    #[error("invalid character '{0}'")]
    InvalidChar(char),
    #[error("unterminated string")]
    UnterminatedString,
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

    pub fn lex_all(self) -> Result<Vec<Token<'a>>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        for item in self {
            match item {
                Ok(ok) => tokens.push(ok),
                Err(err) => errors.push(err),
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
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
                '#' => {
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
        self.eat_while(|c| c.is_ascii_digit() || c == '_');

        let kind = if self.first() == '.' && self.second().is_ascii_digit() {
            self.bump();
            self.bump();
            self.eat_while(|c| c.is_ascii_digit());
            TokenKind::Float
        } else {
            TokenKind::Integer
        };

        let s = self.view();
        Token::new(kind(s), self.make_span())
    }

    fn string(&mut self) -> LexResult<Token<'a>> {
        self.eat_while(|c| !matches!(c, '"' | '\n'));

        if self.first() != '"' {
            let span = self.make_span();
            self.eat_while(|c| c != '"');
            return Err(LexError::new(LexErrorKind::UnterminatedString, span));
        }

        self.bump();
        let s = self.view();
        Ok(Token::new(TokenKind::String(s), self.make_span()))
    }

    fn ident(&mut self) -> Token<'a> {
        self.eat_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let s = self.view();
        let kind = TokenKind::try_kw(s);
        let span = self.make_span();
        Token::new(kind, span)
    }

    pub fn next_token(&mut self) -> Option<LexResult<Token<'a>>> {
        macro_rules! token {
            ($name:ident) => {
                Some(Ok(Token::new(TokenKind::$name, self.make_span())))
            };
            ($e:expr) => {
                Some($e)
            };
            ($tk:ident, $cont:expr => $cont_tk:ident) => {
                if self.first() == $cont {
                    self.bump();
                    token!($cont_tk)
                } else {
                    token!($tk)
                }
            };
        }

        self.skip_whitespace();

        self.byte_start = self.byte;
        let c = self.bump()?;

        match c {
            '(' => token!(LParen),
            ')' => token!(RParen),
            '{' => token!(LBrace),
            '}' => token!(RBrace),
            '[' => token!(LBracket),
            ']' => token!(RBracket),
            '.' => token!(Dot),
            ',' => token!(Comma),
            ':' => token!(Colon),
            ';' => token!(Semicolon),
            '+' => token!(Plus),
            '-' => token!(Minus),
            '*' => token!(Star),
            '/' => token!(Slash),
            '%' => token!(Percent),
            '^' => token!(Caret),

            '=' => token!(Eq, '=' => EqEq),
            '!' => token!(Bang, '=' => BangEq),
            '>' => token!(Greater, '=' => GreaterEq),
            '<' => token!(Less, '=' => LessEq),

            '0'..='9' => Some(Ok(self.number())),
            'a'..='z' | 'A'..='Z' | '_' => Some(Ok(self.ident())),
            '"' => Some(self.string()),

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
