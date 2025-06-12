use kenspan::Span;
use kenvm::builtin::Builtin;
use kenvm::bytecode::{ChunkBuilder, Op};
use kenvm::obj::Function;
use kenvm::value::Value;

use crate::ast::{Expr, ExprKind, InfixOp, PrefixOp};

pub struct Codegen {
    arity: u8,
    stack: usize,
    chunk: ChunkBuilder,
}

impl Codegen {
    #[must_use]
    pub const fn new(arity: u8) -> Self {
        Self {
            arity,
            stack: 0,
            chunk: ChunkBuilder::new(),
        }
    }

    const fn stack_is_empty(&self) -> bool {
        self.stack == 0
    }

    #[must_use]
    pub fn finish(mut self) -> Function {
        self.push_ret();
        let chunk = self.chunk.finish();
        Function::new(self.arity, chunk)
    }

    pub fn compile_expr(&mut self, Expr { kind, span }: Expr) {
        match kind {
            ExprKind::Number(n) => {
                self.compile_number_expr(n, span);
            }
            ExprKind::Prefix { op, expr } => {
                self.compile_prefix_expr(op, *expr, span);
            }
            ExprKind::Infix { op, lhs, rhs } => self.compile_infix_expr(op, *lhs, *rhs, span),
        }
    }

    fn compile_number_expr(&mut self, n: f64, span: Span) {
        self.chunk.push_push(Value::Number(n), span);
        self.stack += 1;
    }

    fn compile_prefix_expr(&mut self, op: PrefixOp, expr: Expr, span: Span) {
        self.compile_expr(expr);
        match op {
            PrefixOp::Neg => self.chunk.push_op(Op::Neg, span),
        };
    }

    fn compile_infix_expr(&mut self, op: InfixOp, lhs: Expr, rhs: Expr, span: Span) {
        self.compile_expr(lhs);
        self.compile_expr(rhs);
        match op {
            InfixOp::Add => self.chunk.push_op(Op::Add, span),
            InfixOp::Sub => self.chunk.push_op(Op::Sub, span),
            InfixOp::Mul => self.chunk.push_op(Op::Mul, span),
            InfixOp::Div => self.chunk.push_op(Op::Div, span),
            InfixOp::Pow => {
                let pow = Value::Builtin(Builtin::pow());
                self.chunk.push_push(pow, span);
                self.chunk.push_op(Op::Call(2), span)
            }
        };
        self.stack -= 1;
    }

    fn push_ret(&mut self) {
        if self.stack_is_empty() {
            self.chunk.push_push(Value::Unit, Span::default());
        }
        self.chunk.push_op(Op::Ret, Span::default());
    }
}
