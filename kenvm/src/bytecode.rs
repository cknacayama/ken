use std::fmt::Display;

use kenspan::Span;

use crate::value::Value;

/// Representation of bytecode instructions.
///
/// `Op` can be used for building a [Chunk].
#[derive(Debug, Clone, Copy)]
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

impl Op {
    const fn encode_code(&self) -> u8 {
        match self {
            Self::Nop => 0,
            Self::Pop => 1,
            Self::Push(_) => 2,
            Self::PushBool(_) => 3,
            Self::PushU32(_) => 4,
            Self::MakeList(_) => 5,
            Self::MakeTuple(_) => 6,
            Self::MakeTable(_) => 7,
            Self::Neg => 8,
            Self::Not => 9,
            Self::Add => 10,
            Self::Sub => 11,
            Self::Mul => 12,
            Self::Div => 13,
            Self::Rem => 14,
            Self::Pow => 15,
            Self::Eq => 16,
            Self::Lt => 17,
            Self::Le => 18,
            Self::Call(_) => 19,
            Self::CallMethod(_, _) => 20,
            Self::AddGlobal => 21,
            Self::LoadIdx => 22,
            Self::StoreIdx => 23,
            Self::Load(_) => 24,
            Self::Store(_) => 25,
            Self::Restore(_) => 26,
            Self::LoadGlobal(_) => 27,
            Self::StoreGlobal(_) => 28,
            Self::Jmp(_) => 29,
            Self::JmpIfNot(_) => 30,
            Self::Ret => 31,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    const fn decode((hi, lo): (usize, usize)) -> Option<Self> {
        const {
            assert!(size_of::<Self>() == size_of::<(usize, usize)>());
            assert!(align_of::<Self>() == align_of::<(usize, usize)>());
        };

        let value = (hi as u128) << 64 | lo as u128;
        let arg = (value >> 8) as usize;
        match value as u8 {
            0 => Some(Self::Nop),

            1 => Some(Self::Pop),
            2 => Some(Self::Push(arg)),

            3 => Some(Self::PushBool((arg & 1) == 1)),
            4 => Some(Self::PushU32(arg as u32)),

            5 => Some(Self::MakeList(arg)),
            6 => Some(Self::MakeTuple(arg)),
            7 => Some(Self::MakeTable(arg)),

            8 => Some(Self::Neg),
            9 => Some(Self::Not),

            10 => Some(Self::Add),
            11 => Some(Self::Sub),
            12 => Some(Self::Mul),
            13 => Some(Self::Div),
            14 => Some(Self::Rem),
            15 => Some(Self::Pow),

            16 => Some(Self::Eq),

            17 => Some(Self::Lt),
            18 => Some(Self::Le),

            19 => Some(Self::Call(arg as u8)),
            20 => Some(Self::CallMethod(arg as u8, (value >> 16) as usize)),

            21 => Some(Self::AddGlobal),

            22 => Some(Self::LoadIdx),
            23 => Some(Self::StoreIdx),

            24 => Some(Self::Load(arg)),
            25 => Some(Self::Store(arg)),

            26 => Some(Self::Restore(arg)),

            27 => Some(Self::LoadGlobal(arg)),
            28 => Some(Self::StoreGlobal(arg)),

            29 => Some(Self::Jmp(arg)),
            30 => Some(Self::JmpIfNot(arg)),

            31 => Some(Self::Ret),
            _ => None,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    const fn encode(&self) -> (usize, usize) {
        const {
            assert!(size_of::<Self>() == size_of::<(usize, usize)>());
            assert!(align_of::<Self>() == align_of::<(usize, usize)>());
        };

        let code = self.encode_code() as u128;
        let encoded = match *self {
            Self::PushBool(b) => code | (b as u128) << 8,
            Self::Call(arg) => code | (arg as u128) << 8,
            Self::CallMethod(arg, name) => code | (arg as u128) << 8 | (name as u128) << 16,
            Self::PushU32(arg) => code | (arg as u128) << 8,
            Self::MakeList(arg)
            | Self::MakeTable(arg)
            | Self::MakeTuple(arg)
            | Self::Push(arg)
            | Self::Load(arg)
            | Self::Store(arg)
            | Self::Restore(arg)
            | Self::LoadGlobal(arg)
            | Self::StoreGlobal(arg)
            | Self::Jmp(arg)
            | Self::JmpIfNot(arg) => code | (arg as u128) << 8,
            _ => code,
        };
        ((encoded >> 64) as usize, encoded as usize)
    }
}

pub struct ChunkBuilder {
    ops:   Vec<(usize, usize)>,
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
    pub fn update_jmp(&mut self, at: usize, new: usize) -> Option<usize> {
        let code = self.ops.get(at).copied().and_then(Op::decode)?;
        match code {
            Op::Jmp(ip) => {
                self.ops[at] = Op::Jmp(new).encode();
                Some(ip)
            }
            Op::JmpIfNot(ip) => {
                self.ops[at] = Op::JmpIfNot(new).encode();
                Some(ip)
            }
            _ => None,
        }
    }

    pub fn push_op(&mut self, op: Op, span: Span) -> usize {
        let cur = self.ops.len();
        self.ops.push(op.encode());
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
    ops:   Box<[(usize, usize)]>,
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

pub(crate) trait Fetch<T> {
    #[must_use]
    fn fetch(&mut self) -> Option<T>;
}

impl Fetch<Span> for OpStream<'_> {
    fn fetch(&mut self) -> Option<Span> {
        self.chunk.fetch_span(self.ip - 1)
    }
}

impl Fetch<(usize, usize)> for OpStream<'_> {
    fn fetch(&mut self) -> Option<(usize, usize)> {
        let byte = self.chunk.ops.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }
}

impl Fetch<Op> for OpStream<'_> {
    fn fetch(&mut self) -> Option<Op> {
        self.fetch().and_then(Op::decode)
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
        while let Some(op) = <Self as Fetch<Op>>::fetch(&mut stream) {
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
