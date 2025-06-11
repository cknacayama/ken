use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Nop,

    Pop,
    Push(u16),

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,

    Call(u8),

    Load(u16),
    Store(u16),

    Jmp(u16),
    JmpIf(u16),

    Ret,
}

#[derive(Debug)]
pub struct ChunkBuilder {
    ops:  Vec<Op>,
    vals: Vec<Value>,
}

impl Default for ChunkBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl ChunkBuilder {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            ops:  Vec::new(),
            vals: Vec::new(),
        }
    }

    #[must_use]
    pub fn finish(self) -> Chunk {
        Chunk {
            ops:  self.ops.into_boxed_slice(),
            vals: self.vals.into_boxed_slice(),
        }
    }

    pub fn push_op(&mut self, op: Op) -> u16 {
        let cur = self.ops.len();
        self.ops.push(op);
        u16::try_from(cur).unwrap()
    }

    fn push_value(&mut self, value: Value) -> u16 {
        let pos = if let Some(pos) = self.vals.iter().position(|v| v == &value) {
            pos
        } else {
            let cur = self.vals.len();
            self.vals.push(value);
            cur
        };

        u16::try_from(pos).unwrap()
    }

    pub fn push_push(&mut self, value: Value) -> u16 {
        let i = self.push_value(value);
        self.push_op(Op::Push(i))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    ops:  Box<[Op]>,
    vals: Box<[Value]>,
}

impl Chunk {
    #[must_use]
    pub fn fetch(&self, i: u16) -> Option<Op> {
        self.ops.get(i as usize).copied()
    }

    #[must_use]
    pub fn fetch_value(&self, i: u16) -> Option<Value> {
        self.vals.get(i as usize).cloned()
    }
}
