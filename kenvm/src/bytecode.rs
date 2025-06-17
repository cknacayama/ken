use std::fmt::Display;

use kenspan::Span;

use crate::value::Value;

#[derive(Clone, Copy)]
#[repr(u8)]
pub(crate) enum OpCode {
    Nop,

    Pop,
    Push,

    PushInt,

    MakeList,
    MakeTuple,

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,
    Lt,
    Le,

    Call,

    AddLocal,
    AddGlobal,

    LoadIdx,
    StoreIdx,

    Load,
    Store,

    Restore,

    LoadGlobal,
    StoreGlobal,

    Jmp,
    JmpIfNot,

    Ret,
}

impl OpCode {
    const fn decode(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Nop),
            1 => Some(Self::Pop),
            2 => Some(Self::Push),
            3 => Some(Self::PushInt),
            4 => Some(Self::MakeList),
            5 => Some(Self::MakeTuple),
            6 => Some(Self::Neg),
            7 => Some(Self::Not),
            8 => Some(Self::Add),
            9 => Some(Self::Sub),
            10 => Some(Self::Mul),
            11 => Some(Self::Div),
            12 => Some(Self::Rem),
            13 => Some(Self::Eq),
            14 => Some(Self::Lt),
            15 => Some(Self::Le),
            16 => Some(Self::Call),
            17 => Some(Self::AddLocal),
            18 => Some(Self::AddGlobal),
            19 => Some(Self::LoadIdx),
            20 => Some(Self::StoreIdx),
            21 => Some(Self::Load),
            22 => Some(Self::Store),
            23 => Some(Self::Restore),
            24 => Some(Self::LoadGlobal),
            25 => Some(Self::StoreGlobal),
            26 => Some(Self::Jmp),
            27 => Some(Self::JmpIfNot),
            28 => Some(Self::Ret),
            _ => None,
        }
    }
}

/// Representation of bytecode instructions.
///
/// `Op` can be used for building a [Chunk].
/// `Op` is not the internal representation of
/// the bytecode.
///
/// # Errors
///
/// The evaluation of an instruction
/// will return `Err` if there are any type
/// mismatches.
#[derive(Debug, Clone, Copy)]
pub enum Op {
    Nop,

    Pop,
    Push(usize),
    PushInt(u32),

    MakeList(usize),
    MakeTuple(usize),

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Eq,

    Lt,
    Le,

    Call(u8),

    AddLocal,
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
    const fn encode_code(&self) -> OpCode {
        match self {
            Self::Nop => OpCode::Nop,
            Self::Pop => OpCode::Pop,
            Self::Push(_) => OpCode::Push,
            Self::PushInt(_) => OpCode::PushInt,
            Self::MakeList(_) => OpCode::MakeList,
            Self::MakeTuple(_) => OpCode::MakeTuple,
            Self::Neg => OpCode::Neg,
            Self::Not => OpCode::Not,
            Self::Add => OpCode::Add,
            Self::Sub => OpCode::Sub,
            Self::Mul => OpCode::Mul,
            Self::Div => OpCode::Div,
            Self::Rem => OpCode::Rem,
            Self::Eq => OpCode::Eq,
            Self::Lt => OpCode::Lt,
            Self::Le => OpCode::Le,
            Self::Call(_) => OpCode::Call,
            Self::AddLocal => OpCode::AddLocal,
            Self::AddGlobal => OpCode::AddGlobal,
            Self::LoadIdx => OpCode::LoadIdx,
            Self::StoreIdx => OpCode::StoreIdx,
            Self::Load(_) => OpCode::Load,
            Self::Store(_) => OpCode::Store,
            Self::Restore(_) => OpCode::Restore,
            Self::LoadGlobal(_) => OpCode::LoadGlobal,
            Self::StoreGlobal(_) => OpCode::StoreGlobal,
            Self::Jmp(_) => OpCode::Jmp,
            Self::JmpIfNot(_) => OpCode::JmpIfNot,
            Self::Ret => OpCode::Ret,
        }
    }

    fn encode_args(&self) -> ChunkBuffer {
        match *self {
            Self::Call(arg) => {
                let mut chunk = ChunkBuffer::new();
                chunk.push(arg);
                chunk
            }
            Self::PushInt(arg) => {
                let mut chunk = ChunkBuffer::new();
                for b in arg.to_ne_bytes() {
                    chunk.push(b);
                }
                chunk
            }
            Self::MakeList(arg)
            | Self::MakeTuple(arg)
            | Self::Push(arg)
            | Self::Load(arg)
            | Self::Store(arg)
            | Self::Restore(arg)
            | Self::LoadGlobal(arg)
            | Self::StoreGlobal(arg)
            | Self::Jmp(arg)
            | Self::JmpIfNot(arg) => {
                let mut chunk = ChunkBuffer::new();
                for b in arg.to_ne_bytes() {
                    chunk.push(b);
                }
                chunk
            }
            _ => ChunkBuffer::new(),
        }
    }
}

#[derive(Clone, Copy)]
struct ChunkBuffer<const N: usize = 8> {
    buf: [u8; N],
    len: usize,
}

impl<const N: usize> ChunkBuffer<N> {
    const fn new() -> Self {
        Self {
            buf: [0; N],
            len: 0,
        }
    }

    const fn push(&mut self, byte: u8) -> bool {
        if self.len >= N {
            false
        } else {
            let i = self.len;
            self.len += 1;
            self.buf[i] = byte;
            true
        }
    }

    fn as_slice(&self) -> &[u8] {
        &self.buf[..self.len]
    }
}

pub struct ChunkBuilder {
    ops:   Vec<u8>,
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
    pub fn update_jmp(&mut self, at: usize, new: usize) -> Option<usize> {
        let code = self.ops.get(at).copied().and_then(OpCode::decode)?;
        if matches!(code, OpCode::Jmp | OpCode::JmpIfNot) {
            let slice = self.ops.get_mut(at + 1..at + 9)?;
            let old = usize::from_ne_bytes(slice.try_into().unwrap());
            for (new, old) in new.to_ne_bytes().into_iter().zip(slice) {
                *old = new;
            }
            Some(old)
        } else {
            None
        }
    }

    pub fn push_op(&mut self, op: Op, span: Span) -> std::ops::Range<usize> {
        let cur = self.ops.len();
        let code = op.encode_code();
        let args = op.encode_args();
        self.ops.push(code as u8);
        self.ops.extend_from_slice(args.as_slice());
        self.spans.extend(std::iter::repeat_n(span, args.len + 1));
        cur..cur + args.len
    }

    fn push_value(&mut self, value: Value) -> usize {
        if let Some(pos) = self.vals.iter().position(|v| v == &value) {
            pos
        } else {
            let cur = self.vals.len();
            self.vals.push(value);
            cur
        }
    }

    pub fn push_push(&mut self, value: Value, span: Span) -> std::ops::Range<usize> {
        let i = self.push_value(value);
        self.push_op(Op::Push(i), span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    ops:   Box<[u8]>,
    spans: Box<[Span]>,
    vals:  Box<[Value]>,
}

impl Chunk {
    pub const fn len(&self) -> usize {
        self.ops.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[must_use]
    fn fetch_span(&self, at: usize) -> Option<Span> {
        self.spans.get(at).copied()
    }

    #[inline]
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

impl Fetch<u8> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<u8> {
        let byte = self.chunk.ops.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }
}

impl Fetch<u32> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<u32> {
        let at = self.ip;
        let slice = self.chunk.ops.get(at..at + size_of::<u32>())?;
        let bytes = slice.try_into().unwrap();
        let word = u32::from_ne_bytes(bytes);
        self.ip += size_of::<u32>();
        Some(word)
    }
}

impl Fetch<usize> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<usize> {
        let at = self.ip;
        let slice = self.chunk.ops.get(at..at + size_of::<usize>())?;
        let bytes = slice.try_into().unwrap();
        let word = usize::from_ne_bytes(bytes);
        self.ip += size_of::<usize>();
        Some(word)
    }
}

impl Fetch<OpCode> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<OpCode> {
        Fetch::<u8>::fetch(self).and_then(OpCode::decode)
    }
}

impl Fetch<Span> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<Span> {
        self.chunk.fetch_span(self.ip - 1)
    }
}

impl Fetch<Op> for OpStream<'_> {
    fn fetch(&mut self) -> Option<Op> {
        let op = self.fetch()?;
        match op {
            OpCode::Nop => Some(Op::Nop),
            OpCode::Pop => Some(Op::Pop),
            OpCode::Push => self.fetch().map(Op::Push),
            OpCode::PushInt => self.fetch().map(Op::PushInt),
            OpCode::MakeList => self.fetch().map(Op::MakeList),
            OpCode::MakeTuple => self.fetch().map(Op::MakeTuple),
            OpCode::Neg => Some(Op::Neg),
            OpCode::Not => Some(Op::Not),
            OpCode::Add => Some(Op::Add),
            OpCode::Sub => Some(Op::Sub),
            OpCode::Mul => Some(Op::Mul),
            OpCode::Div => Some(Op::Div),
            OpCode::Rem => Some(Op::Rem),
            OpCode::Eq => Some(Op::Eq),
            OpCode::Lt => Some(Op::Lt),
            OpCode::Le => Some(Op::Le),
            OpCode::Call => self.fetch().map(Op::Call),
            OpCode::AddLocal => Some(Op::AddLocal),
            OpCode::AddGlobal => Some(Op::AddGlobal),
            OpCode::LoadIdx => Some(Op::LoadIdx),
            OpCode::StoreIdx => Some(Op::StoreIdx),
            OpCode::Load => self.fetch().map(Op::Load),
            OpCode::Store => self.fetch().map(Op::Store),
            OpCode::Restore => self.fetch().map(Op::Restore),
            OpCode::LoadGlobal => self.fetch().map(Op::LoadGlobal),
            OpCode::StoreGlobal => self.fetch().map(Op::StoreGlobal),
            OpCode::Jmp => self.fetch().map(Op::Jmp),
            OpCode::JmpIfNot => self.fetch().map(Op::JmpIfNot),
            OpCode::Ret => Some(Op::Ret),
        }
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

    #[inline]
    pub(crate) const fn jump(&mut self, to: usize) {
        self.ip = to;
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
            writeln!(f, "    {:<max$}    {op:?}", ip)?;
            ip = stream.ip;
        }
        Ok(())
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let stream = OpStream::new(self);
        Display::fmt(&stream, f)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn op_code() {
        for byte in OpCode::Nop as u8..=OpCode::Ret as u8 {
            let code = OpCode::decode(byte).unwrap();
            assert_eq!(code as u8, byte);
        }
    }
}
