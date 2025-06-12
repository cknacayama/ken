use kenspan::{Span, Spand};
use thiserror::Error;

use crate::ast::{Expr, ExprKind, InfixOp, Operator, PrefixOp};
use crate::token::{Token, TokenKind};

#[derive(Error, Debug, Clone, Copy)]
pub enum ParseErrorKind {
    #[error("unexpected end of input")]
    UnexpectedEnd,
    #[error("invalid expression")]
    InvalidExpr,
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

    fn last_span(&self) -> Span {
        self.tokens.last().map(|tk| tk.span).unwrap_or_default()
    }

    fn peek_n(&self, n: usize) -> Option<Token<'a>> {
        self.tokens.get(self.current + n).copied()
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.peek_n(0)
    }

    const fn eat_n(&mut self, n: usize) {
        self.current += n;
    }

    const fn eat(&mut self) {
        self.eat_n(1);
    }

    fn next(&mut self) -> Option<Token<'a>> {
        let tk = self.peek()?;
        self.current += 1;
        Some(tk)
    }

    fn next_or_err(&mut self) -> ParseResult<Token<'a>> {
        self.next()
            .ok_or_else(|| ParseError::new(ParseErrorKind::UnexpectedEnd, self.last_span()))
    }

    fn expect(&mut self, kind: TokenKind<'static>) -> ParseResult<Span> {
        let tk = self
            .next()
            .ok_or_else(|| ParseError::new(ParseErrorKind::Expected(kind), self.last_span()))?;
        let span = tk.span;
        if tk.kind == kind {
            Ok(span)
        } else {
            Err(ParseError::new(ParseErrorKind::Expected(kind), span))
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_prefix()?;
        self.parse_infix(expr, 0)
    }

    fn parse_infix(&mut self, mut lhs: Expr, min: u8) -> ParseResult<Expr> {
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
                .filter(|new| {
                    new.prec() > op.prec() || (new.fix().is_right() && new.prec() == op.prec())
                })
            {
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

    fn parse_prefix(&mut self) -> ParseResult<Expr> {
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
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let tk = self.next_or_err()?;
        let span = tk.span;

        match tk.kind {
            TokenKind::Number(lit) => {
                let kind = ExprKind::Number(lit.parse().unwrap());
                Ok(Expr::new(kind, span))
            }
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }

            _ => Err(ParseError::new(ParseErrorKind::InvalidExpr, span)),
        }
    }
}
