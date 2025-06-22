use std::fmt::Display;

use kenspan::Span;

use crate::value::Value;

/// Representation of bytecode instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Nop,

    Pop,
    Push(usize),

    PushBool(bool),
    PushU32(u32),

    MakeList(usize),
    MakeTuple(usize),
    MakeTable(usize),

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,

    Eq,

    Lt,
    Le,

    Call(u8),
    CallMethod(u8, usize),

    AddGlobal,

    LoadIdx,
    StoreIdx,

    Load(usize),
    Store(usize),

    Restore(usize),

    LoadGlobal(usize),
    StoreGlobal(usize),

    Jmp(usize),
    JmpIfNot(usize),

    Ret,
}

pub struct ChunkBuilder {
    ops:   Vec<Op>,
    spans: Vec<Span>,
    vals:  Vec<Value>,
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
            ops:   Vec::new(),
            spans: Vec::new(),
            vals:  Vec::new(),
        }
    }

    #[must_use]
    pub fn finish(self) -> Chunk {
        Chunk {
            ops:   self.ops.into_boxed_slice(),
            spans: self.spans.into_boxed_slice(),
            vals:  self.vals.into_boxed_slice(),
        }
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.ops.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    pub fn update_jmp(&mut self, at: usize, mut new: usize) -> Option<usize> {
        let code = self.ops.get_mut(at)?;
        match code {
            Op::Jmp(ip) | Op::JmpIfNot(ip) => {
                std::mem::swap(ip, &mut new);
                Some(new)
            }
            _ => None,
        }
    }

    pub fn push_op(&mut self, op: Op, span: Span) -> usize {
        let cur = self.ops.len();
        self.ops.push(op);
        self.spans.push(span);
        cur
    }

    pub fn push_value(&mut self, value: Value) -> usize {
        if let Some(pos) = self.vals.iter().position(|v| v == &value) {
            pos
        } else {
            let cur = self.vals.len();
            self.vals.push(value);
            cur
        }
    }

    pub fn push_push(&mut self, value: Value, span: Span) -> usize {
        let i = self.push_value(value);
        self.push_op(Op::Push(i), span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    ops:   Box<[Op]>,
    spans: Box<[Span]>,
    vals:  Box<[Value]>,
}

impl Chunk {
    #[must_use]
    pub const fn len(&self) -> usize {
        self.ops.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    fn fetch_span(&self, at: usize) -> Option<Span> {
        self.spans.get(at).copied()
    }

    #[must_use]
    fn fetch_value(&self, at: usize) -> Option<Value> {
        self.vals.get(at).cloned()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OpStream<'a> {
    chunk: &'a Chunk,
    ip:    usize,
}

impl OpStream<'_> {
    pub const fn fetch(&mut self) -> Option<Op> {
        let slice = &*self.chunk.ops;
        if self.ip < slice.len() {
            let op = slice[self.ip];
            self.ip += 1;
            Some(op)
        } else {
            None
        }
    }

    pub fn fetch_span(&mut self) -> Option<Span> {
        self.chunk.fetch_span(self.ip - 1)
    }
}

impl<'a> OpStream<'a> {
    #[must_use]
    pub const fn new(chunk: &'a Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    #[must_use]
    pub(crate) fn fetch_value(&self, at: usize) -> Option<Value> {
        self.chunk.fetch_value(at)
    }

    pub(crate) const fn jump(&mut self, to: usize) {
        self.ip = to;
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => write!(f, "nop"),
            Self::Pop => write!(f, "pop"),
            Self::Push(at) => write!(f, "push data[{at}]"),
            Self::PushBool(b) => write!(f, "push {b}"),
            Self::PushU32(x) => write!(f, "push {x}"),
            Self::MakeList(count) => write!(f, "make list[len = {count}]"),
            Self::MakeTuple(count) => write!(f, "make tuple[len = {count}]"),
            Self::MakeTable(count) => write!(f, "make table[len = {count}]"),
            Self::Neg => write!(f, "neg"),
            Self::Not => write!(f, "not"),
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Rem => write!(f, "rem"),
            Self::Pow => write!(f, "pow"),
            Self::Eq => write!(f, "eq"),
            Self::Lt => write!(f, "lt"),
            Self::Le => write!(f, "le"),
            Self::Call(n) => write!(f, "call[count = {n}]"),
            Self::CallMethod(n, method) => write!(f, "call method[data[{method}], count = {n}]"),
            Self::AddGlobal => write!(f, "add global"),
            Self::LoadIdx => write!(f, "load idx"),
            Self::StoreIdx => write!(f, "store idx"),
            Self::Load(at) => write!(f, "load local[{at}]"),
            Self::Store(at) => write!(f, "store local[{at}]"),
            Self::Restore(n) => write!(f, "pop local[count = {n}]"),
            Self::LoadGlobal(at) => write!(f, "load global[{at}]"),
            Self::StoreGlobal(at) => write!(f, "store global[{at}]"),
            Self::Jmp(to) => write!(f, "jmp chunk[{to}]"),
            Self::JmpIfNot(to) => write!(f, "jmp if not chunk[{to}]"),
            Self::Ret => write!(f, "ret"),
        }
    }
}

impl Display for OpStream<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.chunk.is_empty() {
            return Ok(());
        }
        let max = self.chunk.len().ilog10() as usize + 1;
        let mut stream = *self;
        let mut ip = stream.ip;
        while let Some(op) = stream.fetch() {
            writeln!(f, "    {ip:>max$}:    {op}")?;
            ip = stream.ip;
        }
        Ok(())
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stream = OpStream::new(self);
        writeln!(f, "text:")?;
        write!(f, "{stream}")?;
        if self.vals.is_empty() {
            return Ok(());
        }
        writeln!(f, "data:")?;
        let max = self.vals.len().ilog10() as usize + 1;
        for (at, value) in self.vals.iter().enumerate() {
            writeln!(f, "    {at:>max$}:    {value}")?;
        }
        Ok(())
    }
}
