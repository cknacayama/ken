use std::collections::HashMap;

use kenspan::{Span, Spand};
use kenvm::builtin::Builtin;
use kenvm::bytecode::{ChunkBuilder, Op};
use kenvm::obj::Function;
use kenvm::value::Value;

use crate::ast::{
    Block, Expr, ExprKind, Fn, InfixOp, Item, ItemKind, Local, PrefixOp, Stmt, StmtKind,
};

#[derive(Debug)]
pub struct GlobalMap<'src> {
    globals: HashMap<&'src str, usize>,
    current: usize,
}

impl Default for GlobalMap<'_> {
    fn default() -> Self {
        let builtins = Builtin::core_builtins();
        let current = builtins.len();
        let globals = builtins.enumerate().map(|(at, b)| (b.name(), at)).collect();
        Self { globals, current }
    }
}

impl<'a> GlobalMap<'a> {
    fn add(&mut self, name: &'a str) -> usize {
        let at = self.current;
        self.current += 1;
        self.globals.insert(name, at);
        at
    }

    #[must_use]
    fn get(&self, name: &'a str) -> Option<usize> {
        self.globals.get(name).copied()
    }
}

pub struct Codegen<'src, 'glob> {
    name:    &'src str,
    globals: &'glob mut GlobalMap<'src>,
    scope:   Vec<HashMap<&'src str, usize>>,
    locals:  usize,
    arity:   u8,
    stack:   usize,
    chunk:   ChunkBuilder,
}

impl<'src, 'glob> Codegen<'src, 'glob> {
    #[must_use]
    pub const fn new(name: &'src str, arity: u8, globals: &'glob mut GlobalMap<'src>) -> Self {
        Self {
            name,
            arity,
            stack: 0,
            locals: 0,
            globals,
            scope: Vec::new(),
            chunk: ChunkBuilder::new(),
        }
    }

    #[must_use]
    pub const fn function_name(&self) -> &'src str {
        self.name
    }

    const fn stack_is_empty(&self) -> bool {
        self.stack == 0
    }

    fn begin_scope(&mut self) -> usize {
        self.scope.push(HashMap::default());
        self.stack
    }

    fn end_scope(&mut self, span: Span) -> GenSpan {
        let scope = self.scope.pop().unwrap();
        if scope.is_empty() {
            self.push_op(Op::Nop, span)
        } else {
            self.push_op(Op::Restore(scope.len()), span)
        }
    }

    fn add_variable(&mut self, name: &'src str) -> bool {
        if let Some(scope) = self.scope.last_mut() {
            let pos = self.locals;
            self.locals += 1;
            scope.insert(name, pos);
            true
        } else {
            self.globals.add(name);
            false
        }
    }

    fn bind_variable(&mut self, name: &'src str, span: Span) -> GenSpan {
        if self.add_variable(name) {
            self.push_op(Op::AddLocal, span)
        } else {
            self.push_op(Op::AddGlobal, span)
        }
    }

    fn get_global(&self, name: &'src str) -> Option<usize> {
        self.globals.get(name)
    }

    fn get_local(&self, name: &'src str) -> Option<usize> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn redact_jmp(&mut self, i: usize, new: usize) -> usize {
        self.chunk.update_jmp(i, new).unwrap()
    }

    fn push_op(&mut self, op: Op, span: Span) -> GenSpan {
        self.chunk.push_op(op, span).into()
    }

    fn push_push(&mut self, value: Value, span: Span) -> GenSpan {
        self.chunk.push_push(value, span).into()
    }

    fn push_unit(&mut self, span: Span) -> GenSpan {
        let op = self.push_push(Value::Unit, span);
        self.stack += 1;
        op
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

    pub fn compile_stmt(&mut self, Spand { kind, span }: Stmt<'src>) -> CodegenResult<GenSpan> {
        match kind {
            StmtKind::Item(item) => self.compile_item(item),
            StmtKind::Expr(expr) => self.compile_expr(expr),
            StmtKind::Semi(expr) => {
                let expr = self.compile_expr(expr)?;
                let end = self.push_op(Op::Pop, span);
                self.stack -= 1;
                Ok(expr.join(end))
            }
            StmtKind::Empty => {
                let op = self.push_op(Op::Nop, span);
                Ok(op)
            }
        }
    }

    pub fn compile_item(&mut self, Item { kind, span }: Item<'src>) -> CodegenResult<GenSpan> {
        match kind {
            ItemKind::Fn(f) => self.compile_fn(f, span),
            ItemKind::Let(local) => self.compile_local(local, span),
        }
    }

    pub fn compile_expr(&mut self, Expr { kind, span }: Expr<'src>) -> CodegenResult<GenSpan> {
        match kind {
            ExprKind::Ident(id) => self.compile_ident_expr(id, span),
            ExprKind::Float(n) => Ok(self.compile_float_expr(n, span)),
            ExprKind::Integer(n) => Ok(self.compile_int_expr(n, span)),
            ExprKind::Prefix { op, expr } => self.compile_prefix_expr(op, *expr, span),
            ExprKind::Infix { op, lhs, rhs } => self.compile_infix_expr(op, *lhs, *rhs, span),
            ExprKind::Block(block) => self.compile_block(block),
            ExprKind::If { cond, then, els } => self.compile_if(*cond, then, els, span),
            ExprKind::Call { callee, args } => self.compile_call(*callee, args, span),
        }
    }

    fn compile_fn(
        &mut self,
        Fn { name, params, body }: Fn<'src>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let count = u8::try_from(params.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let local = self.add_variable(name);
        let mut codegen = Codegen::new(name, count, self.globals);
        codegen.begin_scope();
        for param in params {
            codegen.add_variable(param);
        }
        let body = codegen.compile_block(body)?;
        codegen.end_scope(span);

        let f = Value::from(codegen.finish());
        self.push_push(f, span);
        let end = if local {
            self.push_op(Op::AddLocal, span)
        } else {
            self.push_op(Op::AddGlobal, span)
        };

        Ok(body.join(end))
    }

    fn compile_local(&mut self, local: Local<'src>, span: Span) -> CodegenResult<GenSpan> {
        let bind = if let Some(bind) = local.bind {
            self.compile_expr(bind)?
        } else {
            self.push_unit(span)
        };

        let end = self.bind_variable(local.name, span);
        self.stack -= 1;

        Ok(bind.join(end))
    }

    fn compile_if(
        &mut self,
        cond: Expr<'src>,
        then: Block<'src>,
        els: Block<'src>,
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

        self.redact_jmp(jmp.start, els.start);
        self.redact_jmp(jmp_end.start, end);

        Ok(cond.join(els))
    }

    fn compile_call(
        &mut self,
        callee: Expr<'src>,
        args: Box<[Expr<'src>]>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let count = u8::try_from(args.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let args = self.compile_many(args, Self::compile_expr)?;

        let callee = self.compile_expr(callee)?;
        let end = self.push_op(Op::Call(count), span);
        self.stack -= count as usize;

        let span = args.map_or_else(|| callee.join(end), |args| args.join(end));
        Ok(span)
    }

    fn compile_block(&mut self, Block { stmts, span }: Block<'src>) -> CodegenResult<GenSpan> {
        let stack = self.begin_scope();
        let mut stmts = self.compile_many(stmts, Self::compile_stmt)?;
        if self.stack == stack {
            let unit = self.push_unit(span);
            stmts = Some(stmts.unwrap_or(unit));
        }
        let end = self.end_scope(span);
        let span = stmts.map_or(end, |span| span.join(end));
        Ok(span)
    }

    fn compile_float_expr(&mut self, x: f64, span: Span) -> GenSpan {
        let op = self.push_push(Value::Float(x), span);
        self.stack += 1;
        op
    }

    fn compile_int_expr(&mut self, x: i64, span: Span) -> GenSpan {
        let op = match u32::try_from(x) {
            Ok(x) => self.push_op(Op::PushInt(x), span),
            Err(_) => self.push_push(Value::Int(x), span),
        };
        self.stack += 1;
        op
    }

    fn compile_ident_expr(&mut self, name: &'src str, span: Span) -> CodegenResult<GenSpan> {
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
        Ok(op)
    }

    fn compile_prefix_expr(
        &mut self,
        op: PrefixOp,
        expr: Expr<'src>,
        span: Span,
    ) -> CodegenResult<GenSpan> {
        let start = self.compile_expr(expr)?;
        let end = match op {
            PrefixOp::Neg => self.push_op(Op::Neg, span),
        };
        Ok(start.join(end))
    }

    fn compile_infix_expr(
        &mut self,
        op: InfixOp,
        lhs: Expr<'src>,
        rhs: Expr<'src>,
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
            InfixOp::Eq => self.push_op(Op::Eq, span),
            InfixOp::Ne => {
                self.push_op(Op::Eq, span);
                self.push_op(Op::Not, span)
            }
            InfixOp::Gt => {
                self.push_op(Op::Le, span);
                self.push_op(Op::Not, span)
            }
            InfixOp::Ge => {
                self.push_op(Op::Lt, span);
                self.push_op(Op::Not, span)
            }
            InfixOp::Lt => self.push_op(Op::Lt, span),
            InfixOp::Le => self.push_op(Op::Le, span),
        };
        self.stack -= 1;
        Ok(start.join(end))
    }

    fn push_ret(&mut self) {
        if self.stack_is_empty() {
            self.push_unit(Span::default());
        }
        self.push_op(Op::Ret, Span::default());
    }
}

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

#[derive(Debug, Clone, Copy)]
pub struct GenSpan {
    start: usize,
    end:   usize,
}

impl GenSpan {
    const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    const fn with_end(self, end: usize) -> Self {
        Self { end, ..self }
    }

    const fn join(self, other: Self) -> Self {
        self.with_end(other.end)
    }
}

impl From<std::ops::Range<usize>> for GenSpan {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self::new(value.start, value.end)
    }
}
