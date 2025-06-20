use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use kenspan::{Span, Spand};
use kenvm::builtin::Builtin;
use kenvm::bytecode::{ChunkBuilder, Op};
use kenvm::obj::Function;
use kenvm::value::Value;

use crate::ast::{
    Block, Expr, ExprKind, Fn, InfixOp, Item, ItemKind, Local, PrefixOp, Stmt, StmtKind, TableEntry,
};

#[derive(Debug)]
pub struct GlobalMap {
    globals: HashMap<Rc<str>, usize>,
    strings: HashSet<Rc<str>>,
    current: usize,
}

impl Default for GlobalMap {
    fn default() -> Self {
        let builtins = Builtin::core_builtins();
        let current = builtins.len();
        let globals = builtins
            .iter()
            .copied()
            .enumerate()
            .map(|(at, b)| (Rc::from(b.name()), at))
            .collect::<HashMap<_, _>>();
        let strings = globals.keys().cloned().collect();
        Self {
            globals,
            strings,
            current,
        }
    }
}

impl GlobalMap {
    fn add(&mut self, name: &str) -> usize {
        let name = self.intern(Cow::Borrowed(name));
        let at = self.current;
        self.current += 1;
        self.globals.insert(name, at);
        at
    }

    #[must_use]
    fn get(&self, name: &str) -> Option<usize> {
        self.globals.get(name).copied()
    }

    #[must_use]
    fn intern(&mut self, s: Cow<'_, str>) -> Rc<str> {
        if let Some(s) = self.strings.get(&*s) {
            s.clone()
        } else {
            let s = Rc::<str>::from(s);
            self.strings.insert(s.clone());
            s
        }
    }
}

pub struct Codegen<'a, 'glob> {
    name:   &'a str,
    global: &'glob mut GlobalMap,
    scope:  Vec<HashMap<&'a str, usize>>,
    local:  usize,
    stack:  usize,
    arity:  u8,
    chunk:  ChunkBuilder,
}

impl<'a, 'glob> Codegen<'a, 'glob> {
    #[must_use]
    pub const fn new(name: &'a str, arity: u8, globals: &'glob mut GlobalMap) -> Self {
        Self {
            name,
            arity,
            stack: 0,
            local: 0,
            global: globals,
            scope: Vec::new(),
            chunk: ChunkBuilder::new(),
        }
    }

    #[must_use]
    pub const fn function_name(&self) -> &'a str {
        self.name
    }

    fn begin_scope(&mut self) -> usize {
        self.scope.push(HashMap::default());
        self.stack
    }

    fn end_scope(&mut self, span: Span) -> Option<usize> {
        let scope = self.scope.pop().unwrap();
        if scope.is_empty() {
            None
        } else {
            Some(self.push_op(Op::Restore(scope.len()), span))
        }
    }

    fn add_variable(&mut self, name: &'a str) -> bool {
        if let Some(scope) = self.scope.last_mut() {
            let pos = self.local + self.stack;
            self.local += 1;
            scope.insert(name, pos);
            false
        } else {
            self.global.add(name);
            true
        }
    }

    fn bind_variable(&mut self, name: &'a str, span: Span) -> Option<usize> {
        if self.add_variable(name) {
            Some(self.push_op(Op::AddGlobal, span))
        } else {
            None
        }
    }

    fn get_global(&self, name: &'a str) -> Option<usize> {
        self.global.get(name)
    }

    fn get_local(&self, name: &'a str) -> Option<usize> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn update_jmp(&mut self, i: usize, new: usize) -> usize {
        self.chunk.update_jmp(i, new).unwrap()
    }

    fn push_op(&mut self, op: Op, span: Span) -> usize {
        self.chunk.push_op(op, span)
    }

    fn push_push(&mut self, value: Value, span: Span) -> usize {
        self.chunk.push_push(value, span)
    }

    const fn inc_stack(&mut self, count: usize) {
        self.stack += count;
    }

    const fn dec_stack(&mut self, count: usize) {
        self.stack = self.stack.checked_sub(count).unwrap();
    }

    fn push_unit(&mut self, span: Span) -> usize {
        let op = self.push_push(Value::Unit, span);
        self.inc_stack(1);
        op
    }

    #[must_use]
    pub fn finish(mut self) -> Function {
        let name = if self.name.is_empty() {
            None
        } else {
            Some(self.global.intern(Cow::Borrowed(self.name)))
        };
        self.push_ret();
        let chunk = self.chunk.finish();
        Function::new(name, self.arity as usize, chunk)
    }

    fn compile_many<T>(
        &mut self,
        ast: impl IntoIterator<Item = T>,
        compile: impl std::ops::Fn(&mut Self, T) -> CodegenResult<usize>,
    ) -> CodegenResult<Option<usize>> {
        let mut span: Option<usize> = None;
        for ast in ast {
            let ast = compile(self, ast)?;
            if span.is_none() {
                span = Some(ast);
            }
        }
        Ok(span)
    }

    pub fn compile_stmt(&mut self, Spand { kind, span }: Stmt<'a>) -> CodegenResult<usize> {
        match kind {
            StmtKind::Item(item) => self.compile_item(item),
            StmtKind::Expr(expr) => self.compile_expr(expr),
            StmtKind::Semi(expr) => {
                let expr = self.compile_expr(expr)?;
                self.push_op(Op::Pop, span);
                self.dec_stack(1);
                Ok(expr)
            }
            StmtKind::Empty => {
                let op = self.push_op(Op::Nop, span);
                Ok(op)
            }
        }
    }

    pub fn compile_item(&mut self, Item { kind, span }: Item<'a>) -> CodegenResult<usize> {
        match kind {
            ItemKind::Fn(f) => self.compile_fn(f, span),
            ItemKind::Let(local) => self.compile_local(local, span),
        }
    }

    pub fn compile_expr(&mut self, Expr { kind, span }: Expr<'a>) -> CodegenResult<usize> {
        match kind {
            ExprKind::Unit => Ok(self.push_unit(span)),
            ExprKind::Bool(b) => Ok(self.compile_bool_expr(b, span)),
            ExprKind::Ident(id) => self.compile_ident_expr(id, span),
            ExprKind::Float(n) => Ok(self.compile_float_expr(n, span)),
            ExprKind::Integer(n) => Ok(self.compile_int_expr(n, span)),
            ExprKind::String(s) => Ok(self.compile_str_expr(s, span)),
            ExprKind::Prefix { op, expr } => self.compile_prefix_expr(op, *expr, span),
            ExprKind::Infix {
                op: InfixOp::Assign,
                lhs,
                rhs,
            } => self.compile_assign(*lhs, *rhs, span),
            ExprKind::Infix { op, lhs, rhs } => self.compile_infix_expr(op, *lhs, *rhs, span),
            ExprKind::Block(block) => self.compile_block(block),
            ExprKind::If { cond, then, els } => self.compile_if(*cond, then, els, span),
            ExprKind::Call { callee, args } => self.compile_call(*callee, args, span),
            ExprKind::While { cond, body } => self.compile_while(*cond, body, span),
            ExprKind::List { tuple, items } => self.compile_list(tuple, items, span),
            ExprKind::Idx { expr, idx } => self.compile_idx(*expr, *idx, span),
            ExprKind::Table(items) => self.compile_table(items, span),
            ExprKind::Field { expr, field } => self.compile_field(*expr, field, span),
        }
    }

    fn compile_fn(
        &mut self,
        Fn { name, params, body }: Fn<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let count = u8::try_from(params.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let global = self.add_variable(name);
        let mut codegen = Codegen::new(name, count, self.global);
        codegen.begin_scope();
        for param in params {
            codegen.add_variable(param);
        }
        let body = codegen.compile_block(body)?;
        codegen.scope.pop().unwrap();

        let f = Value::from(codegen.finish());
        self.push_push(f, span);
        if global {
            self.push_op(Op::AddGlobal, span);
        }

        Ok(body)
    }

    fn compile_local(&mut self, local: Local<'a>, span: Span) -> CodegenResult<usize> {
        let bind = if let Some(bind) = local.bind {
            self.compile_expr(bind)?
        } else {
            self.push_unit(span)
        };

        self.dec_stack(1);
        self.bind_variable(local.name, span);

        Ok(bind)
    }

    fn compile_table(
        &mut self,
        entries: Box<[TableEntry<'a>]>,
        span: Span,
    ) -> CodegenResult<usize> {
        let count = entries.len();
        let entries = self.compile_many(entries, Self::compile_entry)?;
        let end = self.push_op(Op::MakeTable(count), span);
        self.dec_stack(count * 2);
        self.inc_stack(1);
        Ok(entries.unwrap_or(end))
    }

    fn compile_entry(&mut self, entry: TableEntry<'a>) -> CodegenResult<usize> {
        let key = self.compile_expr(entry.key)?;
        self.compile_expr(entry.value)?;
        Ok(key)
    }

    fn compile_field(
        &mut self,
        expr: Expr<'a>,
        field: &'a str,
        span: Span,
    ) -> CodegenResult<usize> {
        let expr = self.compile_expr(expr)?;
        let field = self.global.intern(Cow::Borrowed(field));
        self.push_push(Value::from(field), span);
        self.push_op(Op::LoadIdx, span);
        Ok(expr)
    }

    fn compile_idx(&mut self, expr: Expr<'a>, idx: Expr<'a>, span: Span) -> CodegenResult<usize> {
        let expr = self.compile_expr(expr)?;
        self.compile_expr(idx)?;
        self.push_op(Op::LoadIdx, span);
        self.dec_stack(1);
        Ok(expr)
    }

    fn compile_list(
        &mut self,
        tuple: bool,
        items: Box<[Expr<'a>]>,
        span: Span,
    ) -> CodegenResult<usize> {
        let len = items.len();
        let items = self.compile_many(items, Self::compile_expr)?;
        self.dec_stack(len);
        let end = if tuple {
            self.push_op(Op::MakeTuple(len), span)
        } else {
            self.push_op(Op::MakeList(len), span)
        };
        self.inc_stack(1);
        Ok(items.unwrap_or(end))
    }

    fn compile_while(
        &mut self,
        cond: Expr<'a>,
        body: Block<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let cond = self.compile_expr(cond)?;
        let jmp = self.push_op(Op::JmpIfNot(0), span);
        self.dec_stack(1);
        self.compile_block(body)?;
        self.push_op(Op::Pop, span);
        self.dec_stack(1);
        self.push_op(Op::Jmp(cond), span);
        let end = self.push_unit(span);
        self.update_jmp(jmp, end);
        Ok(cond)
    }

    fn compile_if(
        &mut self,
        cond: Expr<'a>,
        then: Block<'a>,
        els: Block<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let cond = self.compile_expr(cond)?;
        let jmp = self.push_op(Op::JmpIfNot(0), span);
        self.dec_stack(1);

        self.compile_block(then)?;
        let jmp_end = self.push_op(Op::Jmp(0), span);
        self.dec_stack(1);

        let els = self.compile_block(els)?;
        let end = self.chunk.len();

        self.update_jmp(jmp, els);
        self.update_jmp(jmp_end, end);

        Ok(cond)
    }

    fn compile_call(
        &mut self,
        callee: Expr<'a>,
        args: Box<[Expr<'a>]>,
        span: Span,
    ) -> CodegenResult<usize> {
        let (callee, method) = match callee.kind {
            ExprKind::Field { expr, field } => {
                let field = self.global.intern(Cow::Borrowed(field));
                let at = self.chunk.push_value(Value::from(field));
                (self.compile_expr(*expr)?, Some(at))
            }
            _ => (self.compile_expr(callee)?, None),
        };

        let count = u8::try_from(args.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        self.compile_many(args, Self::compile_expr)?;

        if let Some(at) = method {
            self.push_op(Op::CallMethod(count, at), span);
        } else {
            self.push_op(Op::Call(count), span);
        }
        self.dec_stack(count as usize);

        Ok(callee)
    }

    fn compile_block(&mut self, Block { stmts, span }: Block<'a>) -> CodegenResult<usize> {
        let stack = self.begin_scope();
        let stmts = self.compile_many(stmts, Self::compile_stmt)?;
        let stmts = if self.stack == stack {
            let unit = self.push_unit(span);
            stmts.unwrap_or(unit)
        } else {
            stmts.unwrap()
        };
        self.end_scope(span);
        Ok(stmts)
    }

    fn compile_float_expr(&mut self, x: f64, span: Span) -> usize {
        let op = self.push_push(Value::Float(x), span);
        self.inc_stack(1);
        op
    }

    fn compile_int_expr(&mut self, x: u32, span: Span) -> usize {
        self.inc_stack(1);
        self.push_op(Op::PushU32(x), span)
    }

    fn compile_bool_expr(&mut self, b: bool, span: Span) -> usize {
        self.inc_stack(1);
        self.push_op(Op::PushBool(b), span)
    }

    fn compile_str_expr(&mut self, s: Cow<'a, str>, span: Span) -> usize {
        let s = self.global.intern(s);
        let op = self.push_push(Value::from(s), span);
        self.inc_stack(1);
        op
    }

    fn compile_ident_expr(&mut self, name: &'a str, span: Span) -> CodegenResult<usize> {
        let op = if let Some(local) = self.get_local(name) {
            Op::Load(local)
        } else {
            let global = self
                .get_global(name)
                .ok_or_else(|| CodegenError::new(CodegenErrorKind::UndefinedVariable, span))?;
            Op::LoadGlobal(global)
        };
        let op = self.push_op(op, span);
        self.inc_stack(1);
        Ok(op)
    }

    fn compile_place(
        &mut self,
        Spand { kind, span: lspan }: Expr<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        match kind {
            ExprKind::Ident(name) => {
                if let Some(local) = self.get_local(name) {
                    Ok(self.push_op(Op::Store(local), lspan))
                } else if let Some(global) = self.get_global(name) {
                    Ok(self.push_op(Op::StoreGlobal(global), lspan))
                } else {
                    Err(CodegenError::new(
                        CodegenErrorKind::UndefinedVariable,
                        lspan,
                    ))
                }
            }
            ExprKind::Field { expr, field } => {
                let expr = self.compile_expr(*expr)?;
                let field = self.global.intern(Cow::Borrowed(field));
                self.push_push(Value::from(field), span);
                self.push_op(Op::StoreIdx, span);
                self.dec_stack(1);
                Ok(expr)
            }
            ExprKind::Idx { expr, idx } => {
                let expr = self.compile_expr(*expr)?;
                self.compile_expr(*idx)?;
                self.push_op(Op::StoreIdx, span);
                self.dec_stack(2);
                Ok(expr)
            }
            _ => Err(CodegenError::new(CodegenErrorKind::NotAssignable, span)),
        }
    }

    fn compile_assign(&mut self, lhs: Expr<'a>, rhs: Expr<'a>, span: Span) -> CodegenResult<usize> {
        let value = self.compile_expr(rhs)?;
        self.compile_place(lhs, span)?;
        self.dec_stack(1);
        self.push_unit(span);
        Ok(value)
    }

    fn compile_prefix_expr(
        &mut self,
        op: PrefixOp,
        expr: Expr<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let start = self.compile_expr(expr)?;
        match op {
            PrefixOp::Neg => {
                self.push_op(Op::Neg, span);
            }
        }
        Ok(start)
    }

    fn compile_infix_expr(
        &mut self,
        op: InfixOp,
        lhs: Expr<'a>,
        rhs: Expr<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let start = self.compile_expr(lhs)?;
        let _ = self.compile_expr(rhs)?;
        match op {
            InfixOp::Add => self.push_op(Op::Add, span),
            InfixOp::Sub => self.push_op(Op::Sub, span),
            InfixOp::Mul => self.push_op(Op::Mul, span),
            InfixOp::Div => self.push_op(Op::Div, span),
            InfixOp::Rem => self.push_op(Op::Rem, span),
            InfixOp::Pow => self.push_op(Op::Pow, span),
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

            InfixOp::Assign => unreachable!(),
        };
        self.dec_stack(1);
        Ok(start)
    }

    fn push_ret(&mut self) {
        if self.stack == 0 {
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
    #[error("not assignable")]
    NotAssignable,
}

pub type CodegenError = Spand<CodegenErrorKind>;
pub type CodegenResult<T> = Result<T, CodegenError>;
