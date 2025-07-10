use std::borrow::Cow;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use kenspan::{Span, Spand};
use kenvm::Vm;
use kenvm::builtin::Builtin;
use kenvm::bytecode::{ChunkBuilder, Op};
use kenvm::hash::HashValue;
use kenvm::obj::{Function, ObjRef, StrRef};
use kenvm::ty::{Ctor, Ty, TyRef};
use kenvm::value::Value;

use crate::ast::{
    AssignOp, Block, Constructor, Expr, ExprKind, FnData, InfixOp, Item, ItemKind, Let, PrefixOp,
    Stmt, StmtKind, Struct, TableEntry,
};

pub struct GlobalMap {
    globals: HashMap<Rc<str>, usize>,
    current: usize,
}

impl GlobalMap {
    pub fn new(vm: &mut Vm) -> Self {
        let builtins = Builtin::core_builtins();
        let builtin_types = vm.ctx().builtin_types();
        let current = builtins.len() + builtin_types.len();
        let globals = builtins
            .into_iter()
            .map(|b| vm.intern_str(b.name()).take())
            .chain(
                builtin_types
                    .into_iter()
                    .map(|t| t.take().name().clone().take()),
            )
            .enumerate()
            .map(|(at, b)| (b, at))
            .collect::<HashMap<_, _>>();

        Self { globals, current }
    }

    fn add(&mut self, name: Rc<str>) -> usize {
        let at = self.current;
        self.current += 1;
        self.globals.insert(name, at);
        at
    }

    #[must_use]
    fn get(&self, name: &str) -> Option<usize> {
        self.globals.get(name).copied()
    }
}

pub struct Codegen<'a, 'vm, 'glob> {
    vm:     &'vm mut Vm,
    global: &'glob mut GlobalMap,
    name:   Option<&'a str>,
    scope:  Vec<HashMap<&'a str, usize>>,
    local:  usize,
    stack:  usize,
    arity:  u8,
    chunk:  ChunkBuilder,
}

impl<'a, 'vm, 'glob> Codegen<'a, 'vm, 'glob> {
    #[must_use]
    pub const fn new(
        name: Option<&'a str>,
        arity: u8,
        global: &'glob mut GlobalMap,
        vm: &'vm mut Vm,
    ) -> Self {
        Self {
            name,
            arity,
            vm,
            global,
            stack: 0,
            local: 0,
            scope: Vec::new(),
            chunk: ChunkBuilder::new(),
        }
    }

    #[must_use]
    fn intern_str<S>(&mut self, s: S) -> StrRef
    where
        Rc<str>: From<S>,
        S: AsRef<str> + Hash + Eq,
    {
        self.vm.intern_str(s)
    }

    #[must_use]
    fn intern_ty(&mut self, ty: Ty) -> TyRef {
        self.vm.intern_ty(ty)
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
            let name = self.intern_str(name);
            self.global.add(name.as_rc().clone());
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

    fn get_global(&self, name: &str) -> Option<usize> {
        self.global.get(name)
    }

    fn get_local(&self, name: &str) -> Option<usize> {
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
        let name = self.name.map(|name| self.intern_str(name));
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
            ItemKind::Fn(f) => self.compile_fn_item(f),
            ItemKind::Let(local) => self.compile_local(local, span),
            ItemKind::Struct(Struct { ctor, methods }) => self.compile_struct(ctor, methods, span),
            ItemKind::Enum(_) => todo!(),
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
                op: AssignOp::Assign,
                lhs,
                rhs,
            } => self.compile_assign(*lhs, *rhs, span),
            ExprKind::Infix {
                op: AssignOp::InfixAssign(op),
                lhs,
                rhs,
            } => self.compile_infix_assign(op, *lhs, *rhs, span),
            ExprKind::Infix {
                op: AssignOp::Infix(op),
                lhs,
                rhs,
            } => self.compile_infix_expr(op, *lhs, *rhs, span),
            ExprKind::Block(block) => self.compile_block(block),
            ExprKind::If { cond, then, els } => self.compile_if(*cond, then, els, span),
            ExprKind::Call { callee, args } => self.compile_call(*callee, args, span),
            ExprKind::While { cond, body } => self.compile_while(*cond, body, span),
            ExprKind::List { tuple, items } => self.compile_list(tuple, items, span),
            ExprKind::Idx { expr, idx } => self.compile_idx(*expr, *idx, span),
            ExprKind::Table { entries: fields } => self.compile_table(fields, span),
            ExprKind::Field { expr, field } => self.compile_field(*expr, field, span),
            ExprKind::Lambda { params, expr } => self.compile_lambda(params, *expr, span),
        }
    }

    fn compile_struct(
        &mut self,
        Constructor { name, fields, .. }: Constructor<'a>,
        methods: Box<[FnData<'a>]>,
        span: Span,
    ) -> CodegenResult<usize> {
        let interned = self.intern_str(name);
        let mut ty = Ty::new(interned.clone());

        let fields = fields
            .into_iter()
            .map(|field| self.intern_str(field))
            .collect::<Box<_>>();
        let field_count = fields
            .len()
            .try_into()
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let ctor = ObjRef::from(Ctor::new(interned.clone(), interned, fields));
        ty.add_ctor(HashValue::Int(field_count), ctor);

        for FnData {
            name,
            params,
            body,
            span,
        } in methods
        {
            let body = Expr::new(ExprKind::Block(body), span);
            let (_, method) = self.compile_fn::<false>(Some(name), params, body, span)?;
            let name = self.intern_str(name);
            ty.add_method(name, ObjRef::from(method));
        }
        let ty = self.intern_ty(ty);

        let op = self.push_push(Value::from(ty), span);
        self.bind_variable(name, span);

        Ok(op)
    }

    fn compile_fn<const RECURSIVE: bool>(
        &mut self,
        name: Option<&'a str>,
        params: Box<[&'a str]>,
        body: Expr<'a>,
        span: Span,
    ) -> CodegenResult<(bool, Function)> {
        let count = u8::try_from(params.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let f = name.is_some_and(|name| RECURSIVE && self.add_variable(name));
        let mut codegen = Codegen::new(name, count, self.global, self.vm);
        codegen.begin_scope();
        for param in params {
            codegen.add_variable(param);
        }
        codegen.compile_expr(body)?;
        codegen.scope.pop().unwrap();

        Ok((f, codegen.finish()))
    }

    fn compile_fn_item(
        &mut self,
        FnData {
            name,
            params,
            body,
            span,
        }: FnData<'a>,
    ) -> CodegenResult<usize> {
        let body = Expr::new(ExprKind::Block(body), span);
        let (global, f) = self.compile_fn::<true>(Some(name), params, body, span)?;

        let f = Value::from(f);
        let op = self.push_push(f, span);
        if global {
            self.push_op(Op::AddGlobal, span);
        }

        Ok(op)
    }

    fn compile_local(&mut self, local: Let<'a>, span: Span) -> CodegenResult<usize> {
        let bind = if let Some(bind) = local.bind {
            self.compile_expr(bind)?
        } else {
            self.push_unit(span)
        };

        self.dec_stack(1);
        self.bind_variable(local.name, span);

        Ok(bind)
    }

    fn compile_lambda(
        &mut self,
        params: Box<[&'a str]>,
        expr: Expr<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let count = u8::try_from(params.len())
            .map_err(|_| CodegenError::new(CodegenErrorKind::TooManyArgs, span))?;
        let mut codegen = Codegen::new(None, count, self.global, self.vm);
        codegen.begin_scope();
        for param in params {
            codegen.add_variable(param);
        }
        codegen.compile_expr(expr)?;
        codegen.scope.pop().unwrap();

        let f = Value::from(codegen.finish());
        let op = self.push_push(f, span);
        self.inc_stack(1);

        Ok(op)
    }

    fn compile_table(
        &mut self,
        entries: Box<[TableEntry<'a>]>,
        span: Span,
    ) -> CodegenResult<usize> {
        let count = entries.len();
        let entries = self.compile_many(entries, Self::compile_entry)?;
        let make = self.push_op(Op::MakeTable(count), span);
        self.dec_stack(count * 2);
        self.inc_stack(1);
        Ok(entries.unwrap_or(make))
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
        let field = self.intern_str(field);
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
                let field = self.intern_str(field);
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
        self.push_op(Op::PushInt(x), span)
    }

    fn compile_bool_expr(&mut self, b: bool, span: Span) -> usize {
        self.inc_stack(1);
        self.push_op(Op::PushBool(b), span)
    }

    fn compile_str_expr(&mut self, s: Cow<'a, str>, span: Span) -> usize {
        let s = self.intern_str(s);
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
                let field = self.intern_str(field);
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

    fn compile_infix_assign(
        &mut self,
        op: InfixOp,
        lhs: Expr<'a>,
        rhs: Expr<'a>,
        span: Span,
    ) -> CodegenResult<usize> {
        let value = self.compile_expr(lhs.clone())?;
        self.compile_expr(rhs)?;
        self.push_op(Self::infix_to_op(op), span);
        self.dec_stack(1);
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

    const fn infix_to_op(op: InfixOp) -> Op {
        match op {
            InfixOp::Add => Op::Add,
            InfixOp::Sub => Op::Sub,
            InfixOp::Mul => Op::Mul,
            InfixOp::Div => Op::Div,
            InfixOp::Rem => Op::Rem,
            InfixOp::Pow => Op::Pow,
            InfixOp::Eq => Op::Eq,
            InfixOp::Ne => Op::Ne,
            InfixOp::Gt => Op::Gt,
            InfixOp::Ge => Op::Ge,
            InfixOp::Lt => Op::Lt,
            InfixOp::Le => Op::Le,
        }
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
        self.push_op(Self::infix_to_op(op), span);
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
