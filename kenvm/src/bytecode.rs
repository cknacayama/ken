use std::fmt::Display;

use kenspan::Span;

use crate::value::Value;

#[derive(Clone, Copy)]
#[repr(u8)]
enum OpCode {
    Nop,

    Pop,
    Push,

    PushInt,

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Lt,
    Le,

    Call,

    AddLocal,
    AddGlobal,

    Load,
    Store,

    Restore,

    LoadGlobal,
    StoreGlobal,

    Jmp,
    JmpUnless,

    Ret,
}

impl OpCode {
    const fn decode(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Nop),
            1 => Some(Self::Pop),
            2 => Some(Self::Push),
            3 => Some(Self::PushInt),
            4 => Some(Self::Neg),
            5 => Some(Self::Not),
            6 => Some(Self::Add),
            7 => Some(Self::Sub),
            8 => Some(Self::Mul),
            9 => Some(Self::Div),
            10 => Some(Self::Eq),
            11 => Some(Self::Lt),
            12 => Some(Self::Le),
            13 => Some(Self::Call),
            14 => Some(Self::AddLocal),
            15 => Some(Self::AddGlobal),
            16 => Some(Self::Load),
            17 => Some(Self::Store),
            18 => Some(Self::Restore),
            19 => Some(Self::LoadGlobal),
            20 => Some(Self::StoreGlobal),
            21 => Some(Self::Jmp),
            22 => Some(Self::JmpUnless),
            23 => Some(Self::Ret),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Nop,

    Pop,
    Push(usize),

    PushInt(u32),

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Lt,
    Le,

    Call(u8),

    AddLocal,
    AddGlobal,

    Load(usize),
    Store(usize),

    Restore(usize),

    LoadGlobal(usize),
    StoreGlobal(usize),

    Jmp(usize),
    JmpUnless(usize),

    Ret,
}

impl Op {
    const fn encode_code(&self) -> OpCode {
        match self {
            Self::Nop => OpCode::Nop,
            Self::Pop => OpCode::Pop,
            Self::Push(_) => OpCode::Push,
            Self::PushInt(_) => OpCode::PushInt,
            Self::Neg => OpCode::Neg,
            Self::Not => OpCode::Not,
            Self::Add => OpCode::Add,
            Self::Sub => OpCode::Sub,
            Self::Mul => OpCode::Mul,
            Self::Div => OpCode::Div,
            Self::Eq => OpCode::Eq,
            Self::Lt => OpCode::Lt,
            Self::Le => OpCode::Le,
            Self::Call(_) => OpCode::Call,
            Self::AddLocal => OpCode::AddLocal,
            Self::AddGlobal => OpCode::AddGlobal,
            Self::Load(_) => OpCode::Load,
            Self::Store(_) => OpCode::Store,
            Self::Restore(_) => OpCode::Restore,
            Self::LoadGlobal(_) => OpCode::LoadGlobal,
            Self::StoreGlobal(_) => OpCode::StoreGlobal,
            Self::Jmp(_) => OpCode::Jmp,
            Self::JmpUnless(_) => OpCode::JmpUnless,
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
            Self::Push(arg)
            | Self::Load(arg)
            | Self::Store(arg)
            | Self::Restore(arg)
            | Self::LoadGlobal(arg)
            | Self::StoreGlobal(arg)
            | Self::Jmp(arg)
            | Self::JmpUnless(arg) => {
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
        if matches!(code, OpCode::Jmp | OpCode::JmpUnless) {
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
    #[must_use]
    fn fetch_span(&self, at: usize) -> Span {
        self.spans.get(at).copied().unwrap_or_default()
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

impl<'a> OpStream<'a> {
    #[must_use]
    pub const fn new(chunk: &'a Chunk) -> Self {
        Self { chunk, ip: 0 }
    }

    #[must_use]
    pub(crate) fn fetch_span(&self) -> Span {
        self.chunk.fetch_span(self.ip - 1)
    }

    #[inline]
    #[must_use]
    fn fetch_u8(&mut self) -> Option<u8> {
        let byte = self.chunk.ops.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }

    #[inline]
    #[must_use]
    fn fetch_u32(&mut self) -> Option<u32> {
        let word = {
            let this = &self.chunk;
            let at = self.ip;
            let slice = this.ops.get(at..at + size_of::<u32>())?;
            let bytes = slice.try_into().unwrap();
            Some(u32::from_ne_bytes(bytes))
        }?;
        self.ip += size_of::<u32>();
        Some(word)
    }

    #[inline]
    #[must_use]
    fn fetch_arg(&mut self, ctor: impl FnOnce(usize) -> Op) -> Option<Op> {
        let at = self.ip;
        let slice = self.chunk.ops.get(at..at + size_of::<usize>())?;
        let bytes = slice.try_into().unwrap();
        let arg = usize::from_ne_bytes(bytes);
        self.ip += size_of::<usize>();
        Some(ctor(arg))
    }

    #[inline]
    #[must_use]
    pub(crate) fn fetch(&mut self) -> Option<Op> {
        let code = self.fetch_u8().and_then(OpCode::decode)?;

        let op = match code {
            OpCode::Nop => Op::Nop,
            OpCode::Pop => Op::Pop,
            OpCode::Push => self.fetch_arg(Op::Push)?,
            OpCode::Neg => Op::Neg,
            OpCode::Not => Op::Not,
            OpCode::Add => Op::Add,
            OpCode::Sub => Op::Sub,
            OpCode::Mul => Op::Mul,
            OpCode::Div => Op::Div,
            OpCode::Eq => Op::Eq,
            OpCode::Lt => Op::Lt,
            OpCode::Le => Op::Le,
            OpCode::Call => {
                let arg = self.fetch_u8()?;
                Op::Call(arg)
            }
            OpCode::PushInt => {
                let arg = self.fetch_u32()?;
                Op::PushInt(arg)
            }
            OpCode::AddLocal => Op::AddLocal,
            OpCode::AddGlobal => Op::AddGlobal,
            OpCode::Load => self.fetch_arg(Op::Load)?,
            OpCode::Store => self.fetch_arg(Op::Store)?,
            OpCode::Restore => self.fetch_arg(Op::Restore)?,
            OpCode::LoadGlobal => self.fetch_arg(Op::LoadGlobal)?,
            OpCode::StoreGlobal => self.fetch_arg(Op::StoreGlobal)?,
            OpCode::Jmp => self.fetch_arg(Op::Jmp)?,
            OpCode::JmpUnless => self.fetch_arg(Op::JmpUnless)?,
            OpCode::Ret => Op::Ret,
        };

        Some(op)
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

impl Iterator for OpStream<'_> {
    type Item = (usize, Op);

    fn next(&mut self) -> Option<Self::Item> {
        let ip = self.ip;
        self.fetch().map(|op| (ip, op))
    }
}

impl Display for OpStream<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (ip, op) in *self {
            writeln!(f, "{ip}: {op}")?;
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => write!(f, "nop"),
            Self::Pop => write!(f, "pop"),
            Self::Push(arg) => write!(f, "push {arg}"),
            Self::PushInt(arg) => write!(f, "push_int {arg}"),
            Self::Neg => write!(f, "neg"),
            Self::Not => write!(f, "not"),
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Eq => write!(f, "eq"),
            Self::Lt => write!(f, "lt"),
            Self::Le => write!(f, "le"),
            Self::Call(arg) => write!(f, "call {arg}"),
            Self::AddLocal => write!(f, "add_local"),
            Self::AddGlobal => write!(f, "add_global"),
            Self::Load(arg) => write!(f, "load {arg}"),
            Self::Store(arg) => write!(f, "store {arg}"),
            Self::Restore(arg) => write!(f, "restore {arg}"),
            Self::LoadGlobal(arg) => write!(f, "load_global {arg}"),
            Self::StoreGlobal(arg) => write!(f, "store_global {arg}"),
            Self::Jmp(arg) => write!(f, "jmp {arg}"),
            Self::JmpUnless(arg) => write!(f, "jmp_unless {arg}"),
            Self::Ret => write!(f, "ret"),
        }
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
