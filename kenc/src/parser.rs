use std::borrow::Cow;

use kenspan::{Span, Spand};
use thiserror::Error;

use crate::ast::{
    self, Block, Expr, ExprKind, InfixOp, Item, ItemKind, Local, Operator, PrefixOp, Stmt, StmtKind,
};
use crate::token::{Token, TokenKind};

#[derive(Error, Debug, Clone, Copy)]
pub enum ParseErrorKind {
    #[error("unexpected end of input")]
    UnexpectedEnd,
    #[error("invalid escape")]
    InvalidEscape,
    #[error("invalid expression")]
    InvalidExpr,
    #[error("expected expression")]
    ExpectedExpr,
    #[error("expected item")]
    ExpectedItem,
    #[error("expected identifier")]
    ExpectedIdent,
    #[error("expected escape")]
    ExpectedEscape,
    #[error("expected '{0}'")]
    Expected(TokenKind<'static>),
}

pub type ParseError = Spand<ParseErrorKind>;
pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    tokens:  Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub const fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    const fn finished(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn last_span(&self) -> Span {
        self.tokens
            .get(self.current)
            .or_else(|| self.tokens.last())
            .map(|tk| tk.span)
            .unwrap_or_default()
    }

    fn peek_n(&self, n: isize) -> Option<Token<'a>> {
        self.tokens
            .get(self.current.saturating_add_signed(n))
            .copied()
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.peek_n(0)
    }

    fn check<F>(&self, f: F) -> bool
    where
        F: FnOnce(Token<'a>) -> bool,
    {
        self.peek().is_none_or(f)
    }

    fn check_kind(&self, kind: TokenKind<'static>) -> bool {
        self.check(|tk| tk.kind == kind)
    }

    const fn eat_n(&mut self, n: usize) {
        self.current += n;
    }

    const fn eat(&mut self) {
        self.eat_n(1);
    }

    fn next(&mut self) -> ParseResult<Token<'a>> {
        let tk = self
            .peek()
            .ok_or_else(|| ParseError::new(ParseErrorKind::UnexpectedEnd, self.last_span()))?;
        self.current += 1;
        Ok(tk)
    }

    fn next_if_kind(&mut self, tk: TokenKind<'static>) -> Option<Span> {
        match self.peek() {
            Some(Token { kind, span }) if kind == tk => {
                self.eat();
                Some(span)
            }
            _ => None,
        }
    }

    fn expect(&mut self, expect: TokenKind<'static>) -> ParseResult<Span> {
        let Token { kind, span } = self
            .next()
            .map_err(|err| ParseError::new(ParseErrorKind::Expected(expect), err.span))?;
        if kind == expect {
            Ok(span)
        } else {
            Err(ParseError::new(ParseErrorKind::Expected(expect), span))
        }
    }

    fn expect_name(&mut self) -> ParseResult<&'a str> {
        self.expect_ident().map(|id| id.kind)
    }

    fn expect_ident(&mut self) -> ParseResult<Spand<&'a str>> {
        let Token { kind, span } = self
            .next()
            .map_err(|err| ParseError::new(ParseErrorKind::ExpectedIdent, err.span))?;
        match kind {
            TokenKind::Ident(id) => Ok(Spand::new(id, span)),
            _ => Err(ParseError::new(ParseErrorKind::ExpectedIdent, span)),
        }
    }

    fn recover(&mut self) {
        while !(self.finished()
            || self.peek().is_some_and(|tk| tk.kind.can_recover())
            || self
                .peek_n(-1)
                .is_some_and(|tk| tk.kind == TokenKind::Semicolon))
        {
            self.eat();
        }
    }

    pub fn parse_all(&mut self) -> Result<Vec<Stmt<'a>>, Vec<ParseError>> {
        let mut stmts = Vec::new();
        let mut errors = Vec::new();

        while let Some(item) = Iterator::next(self) {
            match item {
                Ok(ok) => stmts.push(ok),
                Err(err) => {
                    errors.push(err);
                    self.recover();
                }
            }
        }

        if errors.is_empty() {
            Ok(stmts)
        } else {
            Err(errors)
        }
    }

    pub fn parse_stmt(&mut self) -> ParseResult<Stmt<'a>> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Semicolon,
                span,
            }) => {
                self.eat();
                Ok(Stmt::new(StmtKind::Empty, span))
            }
            Some(Token { kind, .. }) if kind.can_start_item() => {
                let item = self.parse_item()?;
                let span = item.span;
                let kind = StmtKind::Item(item);
                Ok(Stmt::new(kind, span))
            }
            _ => {
                let expr = self.parse_expr()?;
                if let Some(span) = self.next_if_kind(TokenKind::Semicolon) {
                    let span = expr.span.join(span);
                    let kind = StmtKind::Semi(expr);
                    Ok(Stmt::new(kind, span))
                } else {
                    let span = expr.span;
                    let kind = StmtKind::Expr(expr);
                    Ok(Stmt::new(kind, span))
                }
            }
        }
    }

    pub fn parse_item(&mut self) -> ParseResult<Item<'a>> {
        let Token { kind, span } = self.next()?;
        match kind {
            TokenKind::KwLet => {
                let name = self.expect_name()?;
                let bind = if self.next_if_kind(TokenKind::Eq).is_some() {
                    let expr = self.parse_expr()?;
                    Some(expr)
                } else {
                    None
                };
                let semi = self.expect(TokenKind::Semicolon)?;
                let span = span.join(semi);
                let kind = ItemKind::Let(Local { name, bind });
                Ok(Item::new(kind, span))
            }
            TokenKind::KwFn => {
                let name = self.expect_name()?;
                let (params, _) = self.expect_delimited(Paren, Self::expect_name)?;
                let body = self.expect_block()?;
                let span = span.join(body.span);
                let kind = ItemKind::Fn(ast::Fn {
                    name,
                    params: params.into_boxed_slice(),
                    body,
                });
                Ok(Item::new(kind, span))
            }
            _ => Err(ParseError::new(ParseErrorKind::ExpectedItem, span)),
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr<'a>> {
        let expr = self.parse_prefix()?;
        self.parse_infix(expr, 0)
    }

    fn expect_delimited<D: Delim, T>(
        &mut self,
        delim: D,
        parse: impl Fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<(Vec<T>, Span)> {
        let opening = self.expect(D::opening())?;
        self.parse_delimited(delim, opening, parse)
    }

    fn parse_delimited<D: Delim, T>(
        &mut self,
        _: D,
        opening: Span,
        parse: impl Fn(&mut Self) -> ParseResult<T>,
    ) -> ParseResult<(Vec<T>, Span)> {
        let mut data = Vec::new();

        while !self.check_kind(D::closing()) {
            let res = parse(self)?;
            data.push(res);
            if D::separator().is_some_and(|sep| self.next_if_kind(sep).is_none()) {
                break;
            }
        }

        let closing = self.expect(D::closing())?;
        let span = opening.join(closing);

        Ok((data, span))
    }

    fn expect_block(&mut self) -> ParseResult<Block<'a>> {
        let (stmts, span) = self.expect_delimited(Compound, Self::parse_stmt)?;
        Ok(Block {
            stmts: stmts.into_boxed_slice(),
            span,
        })
    }

    fn parse_infix(&mut self, mut lhs: Expr<'a>, min: u8) -> ParseResult<Expr<'a>> {
        while let Some(op) = self
            .peek()
            .and_then(|tk| InfixOp::from_token(tk.kind))
            .map(Operator::as_data)
            .filter(|op| op.prec() >= min)
        {
            self.eat();
            let mut rhs = self.parse_prefix()?;
            while let Some(new) = self
                .peek()
                .and_then(|tk| InfixOp::from_token(tk.kind))
                .map(Operator::as_data)
            {
                if op.prec() == new.prec() && (op.fix() != op.fix() || op.fix().is_nonfix()) {
                    return Err(ParseError::new(
                        ParseErrorKind::InvalidExpr,
                        self.last_span(),
                    ));
                }
                if op.prec() > new.prec() || (op.prec() == new.prec() && op.fix().is_left()) {
                    break;
                }
                let min = op.prec() + u8::from(new.prec() > op.prec());
                rhs = self.parse_infix(rhs, min)?;
            }
            let span = lhs.span.join(rhs.span);
            let kind = ExprKind::Infix {
                op:  op.op(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
            lhs = Expr::new(kind, span);
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> ParseResult<Expr<'a>> {
        match self.peek() {
            Some(Token {
                kind: TokenKind::Minus,
                span,
            }) => {
                self.eat();
                let expr = self.parse_prefix()?;
                let span = span.join(expr.span);
                let kind = ExprKind::Prefix {
                    op:   PrefixOp::Neg,
                    expr: Box::new(expr),
                };
                Ok(Expr::new(kind, span))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> ParseResult<Expr<'a>> {
        let mut expr = self.parse_primary()?;
        loop {
            match self.peek() {
                Some(Token {
                    kind: TokenKind::LParen,
                    span,
                }) => {
                    self.eat();
                    let (args, span) = self.parse_delimited(Paren, span, Self::parse_expr)?;
                    let span = expr.span.join(span);
                    let kind = ExprKind::Call {
                        callee: Box::new(expr),
                        args:   args.into_boxed_slice(),
                    };
                    expr = Expr::new(kind, span);
                }
                Some(Token {
                    kind: TokenKind::LBracket,
                    ..
                }) => {
                    self.eat();
                    let idx = self.parse_expr()?;
                    let span = expr.span.join(self.expect(TokenKind::RBracket)?);
                    let kind = ExprKind::Idx {
                        expr: Box::new(expr),
                        idx:  Box::new(idx),
                    };
                    expr = Expr::new(kind, span);
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_primary(&mut self) -> ParseResult<Expr<'a>> {
        let Token { kind, span } = self.next()?;

        match kind {
            TokenKind::KwIf => {
                let cond = self.parse_expr()?;
                let then = self.expect_block()?;
                self.expect(TokenKind::KwElse)?;
                let els = self.expect_block()?;
                let span = span.join(els.span);
                let kind = ExprKind::If {
                    cond: Box::new(cond),
                    then,
                    els,
                };
                Ok(Expr::new(kind, span))
            }
            TokenKind::KwWhile => {
                let cond = self.parse_expr()?;
                let body = self.expect_block()?;
                let span = span.join(body.span);
                let kind = ExprKind::While {
                    cond: Box::new(cond),
                    body,
                };
                Ok(Expr::new(kind, span))
            }
            TokenKind::LBrace => {
                let (stmts, span) = self.parse_delimited(Compound, span, Self::parse_stmt)?;
                let block = Block {
                    stmts: stmts.into_boxed_slice(),
                    span,
                };
                let kind = ExprKind::Block(block);
                Ok(Expr::new(kind, span))
            }
            TokenKind::LBracket => {
                let (items, span) = self.parse_delimited(Bracket, span, Self::parse_expr)?;
                let kind = ExprKind::List {
                    tuple: false,
                    items: items.into_boxed_slice(),
                };
                Ok(Expr::new(kind, span))
            }
            TokenKind::LParen => {
                let (mut items, span) = self.parse_delimited(Paren, span, Self::parse_expr)?;

                let kind = match items.len() {
                    0 => ExprKind::Unit,
                    1 => items.pop().unwrap().kind,
                    _ => ExprKind::List {
                        tuple: true,
                        items: items.into_boxed_slice(),
                    },
                };

                Ok(Expr::new(kind, span))
            }
            TokenKind::String(s) => Self::parse_string(s, span),
            TokenKind::Ident(id) => {
                let kind = ExprKind::Ident(id);
                Ok(Expr::new(kind, span))
            }
            TokenKind::Float(lit) => {
                let kind = ExprKind::Float(lit.parse().unwrap());
                Ok(Expr::new(kind, span))
            }
            TokenKind::Integer(lit) => {
                let kind = ExprKind::Integer(lit.parse().unwrap());
                Ok(Expr::new(kind, span))
            }

            _ => Err(ParseError::new(ParseErrorKind::InvalidExpr, span)),
        }
    }

    fn parse_string(s: &'a str, span: Span) -> ParseResult<Expr<'a>> {
        let s = &s[1..s.len() - 1];

        if !s.contains('\\') {
            let kind = ExprKind::String(Cow::Borrowed(s));
            return Ok(Expr::new(kind, span));
        }

        let mut buf = String::new();
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            let c = if c == '\\' {
                let c = chars
                    .next()
                    .ok_or_else(|| ParseError::new(ParseErrorKind::ExpectedEscape, span))?;
                match c {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '0' => '\0',
                    '\'' => '\'',
                    '"' => '"',
                    _ => return Err(ParseError::new(ParseErrorKind::InvalidEscape, span)),
                }
            } else {
                c
            };
            buf.push(c);
        }

        let kind = ExprKind::String(Cow::Owned(buf));
        Ok(Expr::new(kind, span))
    }
}

trait Delim {
    fn opening() -> TokenKind<'static>;
    fn closing() -> TokenKind<'static>;
    fn separator() -> Option<TokenKind<'static>>;
}

struct Paren;
struct Bracket;
struct Compound;

impl Delim for Paren {
    fn opening() -> TokenKind<'static> {
        TokenKind::LParen
    }
    fn closing() -> TokenKind<'static> {
        TokenKind::RParen
    }
    fn separator() -> Option<TokenKind<'static>> {
        Some(TokenKind::Comma)
    }
}

impl Delim for Bracket {
    fn opening() -> TokenKind<'static> {
        TokenKind::LBracket
    }
    fn closing() -> TokenKind<'static> {
        TokenKind::RBracket
    }
    fn separator() -> Option<TokenKind<'static>> {
        Some(TokenKind::Comma)
    }
}

impl Delim for Compound {
    fn opening() -> TokenKind<'static> {
        TokenKind::LBrace
    }
    fn closing() -> TokenKind<'static> {
        TokenKind::RBrace
    }
    fn separator() -> Option<TokenKind<'static>> {
        None
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = ParseResult<Stmt<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished() {
            None
        } else {
            Some(self.parse_stmt())
        }
    }
}
