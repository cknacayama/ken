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
    #[error("too many variables")]
    TooManyVariables,
    #[error("too many instructions")]
    TooManyInstructions,
    #[error("not jmp")]
    NotJmp,
    #[error("scope error")]
    ScopeError,
}

pub type CodegenError = Spand<CodegenErrorKind>;
pub type CodegenResult<T> = Result<T, CodegenError>;

pub struct Codegen<'a> {
    globals: HashMap<&'a str, u16>,
    scope:   Vec<HashMap<&'a str, u16>>,
    locals:  u16,
    arity:   u8,
    stack:   usize,
    chunk:   ChunkBuilder,
}

#[derive(Debug, Clone, Copy)]
pub struct GenSpan {
    start: u16,
    end:   u16,
}

impl GenSpan {
    const fn new(start: u16, end: u16) -> Self {
        Self { start, end }
    }

    const fn single(op: u16) -> Self {
        Self::new(op, op)
    }

    const fn with_end(self, end: u16) -> Self {
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

    fn begin_scope(&mut self) {
        self.scope.push(HashMap::default());
    }

    fn end_scope(&mut self) -> Option<HashMap<&'a str, u16>> {
        self.scope.pop()
    }

    fn add_local(&mut self, name: &'a str, span: Span) -> CodegenResult<()> {
        if self.locals == u16::MAX {
            return Err(CodegenError::new(CodegenErrorKind::TooManyVariables, span));
        }
        let pos = self.locals;
        self.locals += 1;
        self.scope
            .last_mut()
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::ScopeError, span))?
            .insert(name, pos);
        Ok(())
    }

    fn get_global(&self, name: &'a str) -> Option<u16> {
        self.globals.get(name).copied()
    }

    fn get_local(&self, name: &'a str) -> Option<u16> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn redact_jmp(&mut self, i: u16, new: u16, span: Span) -> CodegenResult<u16> {
        self.chunk
            .redact_jmp(i, new)
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::NotJmp, span))
    }

    fn push_op(&mut self, op: Op, span: Span) -> CodegenResult<u16> {
        self.chunk
            .push_op(op, span)
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::TooManyInstructions, span))
    }

    fn push_push(&mut self, value: Value, span: Span) -> CodegenResult<u16> {
        self.chunk
            .push_push(value, span)
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::TooManyInstructions, span))
    }

    #[must_use]
    pub fn finish(mut self) -> Function {
        self.push_ret();
        let chunk = self.chunk.finish();
        Function::new(self.arity, chunk)
    }

    pub fn compile_stmt(&mut self, Spand { kind, span }: Stmt<'a>) -> CodegenResult<GenSpan> {
        match kind {
            StmtKind::Item(item) => self.compile_item(item),
            StmtKind::Expr(expr) => self.compile_expr(expr),
            StmtKind::Semi(expr) => {
                let expr = self.compile_expr(expr)?;
                let end = self.push_op(Op::Pop, span)?;
                Ok(expr.with_end(end))
            }
            StmtKind::Empty => self.push_op(Op::Nop, span).map(GenSpan::single),
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
            self.push_push(Value::Unit, span).map(GenSpan::single)?
        };

        self.add_local(local.name, span)?;
        let end = self.push_op(Op::AddLocal, span)?;

        Ok(bind.with_end(end))
    }

    pub fn compile_expr(&mut self, Expr { kind, span }: Expr<'a>) -> CodegenResult<GenSpan> {
        match kind {
            ExprKind::Ident(id) => self.compile_ident_expr(id, span),
            ExprKind::Number(n) => self.compile_number_expr(n, span),
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
        let jmp = self.push_op(Op::JmpUnless(0), span)?;

        self.compile_block(then)?;
        let jmp_end = self.push_op(Op::Jmp(0), span)?;

        let els = self.compile_block(els)?;
        let end = els
            .end
            .checked_add(1)
            .ok_or_else(|| CodegenError::new(CodegenErrorKind::TooManyInstructions, span))?;

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
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyVariables, span))?;
        let args = args
            .into_iter()
            .map(|arg| self.compile_expr(arg))
            .reduce(|acc, x| acc.and_then(|acc| x.map(|x| acc.join(x))))
            .transpose()?;

        let callee = self.compile_expr(callee)?;
        let end = self.push_op(Op::Call(count), span)?;

        let span = args.map_or_else(|| callee.with_end(end), |args| args.with_end(end));
        Ok(span)
    }

    fn compile_block(&mut self, Block { stmts, span }: Block<'a>) -> CodegenResult<GenSpan> {
        todo!()
    }

    fn compile_number_expr(&mut self, n: f64, span: Span) -> CodegenResult<GenSpan> {
        let op = self.push_push(Value::Number(n), span)?;
        self.stack += 1;
        Ok(GenSpan::single(op))
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
        let op = self.push_op(op, span)?;
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
            PrefixOp::Neg => self.push_op(Op::Neg, span)?,
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
        self.compile_expr(rhs)?;
        let end = match op {
            InfixOp::Add => self.push_op(Op::Add, span)?,
            InfixOp::Sub => self.push_op(Op::Sub, span)?,
            InfixOp::Mul => self.push_op(Op::Mul, span)?,
            InfixOp::Div => self.push_op(Op::Div, span)?,
            InfixOp::Pow => {
                let pow = Value::Builtin(Builtin::pow());
                self.push_push(pow, span)?;
                self.push_op(Op::Call(2), span)?
            }
        };
        self.stack -= 1;
        Ok(start.with_end(end))
    }

    fn push_ret(&mut self) {
        if self.stack_is_empty() {
            self.chunk.push_push(Value::Unit, Span::default());
        }
        self.chunk.push_op(Op::Ret, Span::default());
    }
}
