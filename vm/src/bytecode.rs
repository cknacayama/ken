use crate::value::{Function, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Nop,

    Pop,
    Push(u16),

    Add,

    Call(u8),

    Load(u16),
    Store(u16),

    Exit,
}

#[derive(Debug)]
pub struct Chunk {
    ops:  Vec<Op>,
    vals: Vec<Value>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            ops:  Vec::new(),
            vals: Vec::new(),
        }
    }

    #[must_use]
    pub fn fetch(&self, i: u16) -> Option<Op> {
        self.ops.get(i as usize).copied()
    }

    #[must_use]
    pub fn fetch_value(&self, i: u16) -> Option<Value> {
        self.vals.get(i as usize).cloned()
    }

    fn push_op(&mut self, op: Op) -> u16 {
        let cur = self.ops.len();
        self.ops.push(op);
        u16::try_from(cur).unwrap()
    }

    fn push_value(&mut self, value: Value) -> u16 {
        let cur = self.ops.len();
        self.vals.push(value);
        u16::try_from(cur).unwrap()
    }

    pub fn push_add(&mut self) -> u16 {
        self.push_op(Op::Add)
    }

    pub fn push_call(&mut self, arg_count: u8) -> u16 {
        self.push_op(Op::Call(arg_count))
    }

    pub fn push_load(&mut self, local: u16) -> u16 {
        self.push_op(Op::Load(local))
    }

    pub fn push_store(&mut self, local: u16) -> u16 {
        self.push_op(Op::Store(local))
    }

    pub fn push_pop(&mut self) -> u16 {
        self.push_op(Op::Pop)
    }

    pub fn push_push(&mut self, value: Value) -> u16 {
        let i = self.push_value(value);
        self.push_op(Op::Push(i))
    }

    pub fn push_exit(&mut self) -> u16 {
        self.push_op(Op::Exit)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Frame<'a> {
    function: &'a Function,
    ip:       u16,
    bp:       u32,
}

impl<'a> Frame<'a> {
    #[must_use]
    pub const fn new(function: &'a Function, bp: u32) -> Self {
        Self {
            function,
            bp,
            ip: 0,
        }
    }

    pub fn fetch(&mut self) -> Option<Op> {
        let op = self.function.chunk().fetch(self.ip);
        self.ip += 1;
        op
    }

    #[must_use]
    pub fn fetch_value(&self, i: u16) -> Option<Value> {
        self.function.chunk().fetch_value(i)
    }

    pub const fn jump(&mut self, i: u16) {
        self.ip = i;
    }

    #[must_use]
    pub const fn bp(&self) -> u32 {
        self.bp
    }
}
