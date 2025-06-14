use std::collections::HashMap;

use kenspan::{Span, Spand};
use kenvm::builtin::Builtin;
use kenvm::bytecode::{ChunkBuilder, Op};
use kenvm::obj::Function;
use kenvm::value::Value;

use crate::ast::{
    Block, Expr, ExprKind, Fn, InfixOp, Item, ItemKind, Local, PrefixOp, Stmt, StmtKind,
};

#[derive(thiserror::Error, Debug, Clone, Copy)]
pub enum CodegenErrorKind {
    #[error("undefined variable")]
    UndefinedVariable,
    #[error("too many arguments")]
    TooManyArgs,
    #[error("not jmp")]
    NotJmp,
    #[error("scope error")]
    ScopeError,
}

pub type CodegenError = Spand<CodegenErrorKind>;
pub type CodegenResult<T> = Result<T, CodegenError>;

pub struct Codegen<'a> {
    globals: HashMap<&'a str, usize>,
    scope:   Vec<HashMap<&'a str, usize>>,
    locals:  usize,
    arity:   u8,
    stack:   usize,
    chunk:   ChunkBuilder,
}

#[derive(Debug, Clone, Copy)]
pub struct GenSpan {
    start: usize,
    end:   usize,
}

impl GenSpan {
    const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    const fn single(op: usize) -> Self {
        Self::new(op, op)
    }

    const fn with_end(self, end: usize) -> Self {
        Self { end, ..self }
    }

    const fn join(self, other: Self) -> Self {
        self.with_end(other.end)
    }
}

impl<'a> Codegen<'a> {
    #[must_use]
    pub fn new(arity: u8) -> Self {
        Self {
            arity,
            stack: 0,
            locals: 0,
            globals: HashMap::default(),
            scope: Vec::new(),
            chunk: ChunkBuilder::new(),
        }
    }

    const fn stack_is_empty(&self) -> bool {
        self.stack == 0
    }

    fn begin_scope(&mut self) -> usize {
        self.scope.push(HashMap::default());
        self.stack
    }

    fn end_scope(&mut self) -> Option<HashMap<&'a str, usize>> {
        self.scope.pop()
    }

    fn add_local(&mut self, name: &'a str, span: Span) -> CodegenResult<()> {
        let pos = self.locals;
        self.locals += 1;
        self.scope
            .last_mut()
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::ScopeError, span))?
            .insert(name, pos);
        Ok(())
    }

    fn get_global(&self, name: &'a str) -> Option<usize> {
        self.globals.get(name).copied()
    }

    fn get_local(&self, name: &'a str) -> Option<usize> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn redact_jmp(&mut self, i: usize, new: usize, span: Span) -> CodegenResult<usize> {
        self.chunk
            .redact_jmp(i, new)
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::NotJmp, span))
    }

    fn push_op(&mut self, op: Op, span: Span) -> usize {
        self.chunk.push_op(op, span)
    }

    fn push_push(&mut self, value: Value, span: Span) -> usize {
        self.chunk.push_push(value, span)
    }

    fn push_unit(&mut self, span: Span) -> GenSpan {
        let op = self.push_push(Value::Unit, span);
        self.stack += 1;
        GenSpan::single(op)
    }

    #[must_use]
    pub fn finish(mut self) -> Function {
        self.push_ret();
        let chunk = self.chunk.finish();
        Function::new(self.arity, chunk)
    }

    fn compile_many<T>(
        &mut self,
        ast: impl IntoIterator<Item = T>,
        compile: impl std::ops::Fn(&mut Self, T) -> CodegenResult<GenSpan>,
    ) -> CodegenResult<Option<GenSpan>> {
        let mut span: Option<GenSpan> = None;
        for ast in ast {
            let ast = compile(self, ast)?;
            if let Some(ref mut span) = span {
                *span = span.join(ast);
            } else {
                span = Some(ast);
            }
        }
        Ok(span)
    }

    pub fn compile_stmt(&mut self, Spand { kind, span }: Stmt<'a>) -> CodegenResult<GenSpan> {
        match kind {
            StmtKind::Item(item) => self.compile_item(item),
            StmtKind::Expr(expr) => self.compile_expr(expr),
            StmtKind::Semi(expr) => {
                let expr = self.compile_expr(expr)?;
                let end = self.push_op(Op::Pop, span);
                self.stack -= 1;
                Ok(expr.with_end(end))
            }
            StmtKind::Empty => {
                let op = self.push_op(Op::Nop, span);
                Ok(GenSpan::single(op))
            }
        }
    }

    pub fn compile_item(&mut self, Item { kind, span }: Item<'a>) -> CodegenResult<GenSpan> {
        match kind {
            ItemKind::Fn(f) => self.compile_fn(f, span),
            ItemKind::Let(local) => self.compile_local(local, span),
        }
    }

    fn compile_fn(&mut self, f: Fn<'a>, span: Span) -> CodegenResult<GenSpan> {
        todo!()
    }

    fn compile_local(&mut self, local: Local<'a>, span: Span) -> CodegenResult<GenSpan> {
        let bind = if let Some(bind) = local.bind {
            self.compile_expr(bind)?
        } else {
            self.push_unit(span)
        };

        self.add_local(local.name, span)?;
        let end = self.push_op(Op::AddLocal, span);
        self.stack -= 1;

        Ok(bind.with_end(end))
    }

    pub fn compile_expr(&mut self, Expr { kind, span }: Expr<'a>) -> CodegenResult<GenSpan> {
        match kind {
            ExprKind::Ident(id) => self.compile_ident_expr(id, span),
            ExprKind::Number(n) => Ok(self.compile_number_expr(n, span)),
            ExprKind::Prefix { op, expr } => self.compile_prefix_expr(op, *expr, span),
            ExprKind::Infix { op, lhs, rhs } => self.compile_infix_expr(op, *lhs, *rhs, span),
            ExprKind::Block(block) => self.compile_block(block),
            ExprKind::If { cond, then, els } => self.compile_if(*cond, then, els, span),
            ExprKind::Call { callee, args } => self.compile_call(*callee, args, span),
        }
    }

    fn compile_if(
        &mut self,
        cond: Expr<'a>,
        then: Block<'a>,
        els: Block<'a>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let cond = self.compile_expr(cond)?;
        let jmp = self.push_op(Op::JmpUnless(0), span);
        self.stack -= 1;

        self.compile_block(then)?;
        let jmp_end = self.push_op(Op::Jmp(0), span);
        self.stack -= 1;

        let els = self.compile_block(els)?;
        let end = els.end + 1;

        self.redact_jmp(jmp, els.start, span)?;
        self.redact_jmp(jmp_end, end, span)?;

        Ok(cond.join(els))
    }

    fn compile_call(
        &mut self,
        callee: Expr<'a>,
        args: Box<[Expr<'a>]>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let count = u8::try_from(args.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let args = self.compile_many(args, Self::compile_expr)?;

        let callee = self.compile_expr(callee)?;
        let end = self.push_op(Op::Call(count), span);
        self.stack -= count as usize;

        let span = args.map_or_else(|| callee.with_end(end), |args| args.with_end(end));
        Ok(span)
    }

    fn compile_block(&mut self, Block { stmts, span }: Block<'a>) -> CodegenResult<GenSpan> {
        let stack = self.begin_scope();
        let mut stmts = self.compile_many(stmts, Self::compile_stmt)?;
        if self.stack == stack {
            let unit = self.push_unit(span);
            stmts = Some(stmts.unwrap_or(unit));
        }
        let scope = self.end_scope().unwrap();
        let locals = scope.len();
        let end = self.push_op(Op::Restore(locals), span);
        let span = stmts.map_or_else(|| GenSpan::single(end), |span| span.with_end(end));
        Ok(span)
    }

    fn compile_number_expr(&mut self, n: f64, span: Span) -> GenSpan {
        let op = self.push_push(Value::Number(n), span);
        self.stack += 1;
        GenSpan::single(op)
    }

    fn compile_ident_expr(&mut self, name: &str, span: Span) -> CodegenResult<GenSpan> {
        let op = if let Some(local) = self.get_local(name) {
            Op::Load(local)
        } else {
            let global = self
                .get_global(name)
                .ok_or_else(|| CodegenError::new(CodegenErrorKind::UndefinedVariable, span))?;
            Op::LoadGlobal(global)
        };
        let op = self.push_op(op, span);
        self.stack += 1;
        Ok(GenSpan::single(op))
    }

    fn compile_prefix_expr(
        &mut self,
        op: PrefixOp,
        expr: Expr<'a>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let start = self.compile_expr(expr)?;
        let end = match op {
            PrefixOp::Neg => self.push_op(Op::Neg, span),
        };
        Ok(start.with_end(end))
    }

    fn compile_infix_expr(
        &mut self,
        op: InfixOp,
        lhs: Expr<'a>,
        rhs: Expr<'a>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let start = self.compile_expr(lhs)?;
        let _ = self.compile_expr(rhs)?;
        let end = match op {
            InfixOp::Add => self.push_op(Op::Add, span),
            InfixOp::Sub => self.push_op(Op::Sub, span),
            InfixOp::Mul => self.push_op(Op::Mul, span),
            InfixOp::Div => self.push_op(Op::Div, span),
            InfixOp::Pow => {
                let pow = Value::Builtin(Builtin::pow());
                self.push_push(pow, span);
                self.push_op(Op::Call(2), span)
            }
        };
        self.stack -= 1;
        Ok(start.with_end(end))
    }

    fn push_ret(&mut self) {
        if self.stack_is_empty() {
            self.push_unit(Span::default());
        }
        self.push_op(Op::Ret, Span::default());
    }
}
