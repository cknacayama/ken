use kenspan::Span;

use crate::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum OpCode {
    Nop,

    Pop,
    Push,

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,

    Call,

    AddLocal,

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
    pub const fn decode(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Nop),
            1 => Some(Self::Pop),
            2 => Some(Self::Push),
            3 => Some(Self::Neg),
            4 => Some(Self::Not),
            5 => Some(Self::Add),
            6 => Some(Self::Sub),
            7 => Some(Self::Mul),
            8 => Some(Self::Div),
            9 => Some(Self::Call),
            10 => Some(Self::AddLocal),
            11 => Some(Self::Load),
            12 => Some(Self::Store),
            13 => Some(Self::Restore),
            14 => Some(Self::LoadGlobal),
            15 => Some(Self::StoreGlobal),
            16 => Some(Self::Jmp),
            17 => Some(Self::JmpUnless),
            18 => Some(Self::Ret),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Nop,

    Pop,
    Push(usize),

    Neg,
    Not,

    Add,
    Sub,
    Mul,
    Div,

    Call(u8),

    AddLocal,

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
            Self::Neg => OpCode::Neg,
            Self::Not => OpCode::Not,
            Self::Add => OpCode::Add,
            Self::Sub => OpCode::Sub,
            Self::Mul => OpCode::Mul,
            Self::Div => OpCode::Div,
            Self::Call(_) => OpCode::Call,
            Self::AddLocal => OpCode::AddLocal,
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

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
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
    pub fn redact_jmp(&mut self, i: usize, new: usize) -> Option<usize> {
        let code = self.ops.get(i).copied().and_then(OpCode::decode)?;
        if matches!(code, OpCode::Jmp | OpCode::JmpUnless) {
            let slice = self.ops.get_mut(i + 1..i + 9)?;
            let old = usize::from_ne_bytes(slice.try_into().unwrap());
            for (new, old) in new.to_ne_bytes().into_iter().zip(slice) {
                *old = new;
            }
            Some(old)
        } else {
            None
        }
    }

    #[inline]
    pub fn push_op(&mut self, op: Op, span: Span) -> usize {
        let cur = self.ops.len();
        let code = op.encode_code();
        let args = op.encode_args();
        self.ops.push(code as u8);
        self.ops.extend_from_slice(args.as_slice());
        self.spans.push(span);
        self.spans.extend(std::iter::repeat_n(span, args.len));
        cur
    }

    #[inline]
    fn push_value(&mut self, value: Value) -> usize {
        if let Some(pos) = self.vals.iter().position(|v| v == &value) {
            pos
        } else {
            let cur = self.vals.len();
            self.vals.push(value);
            cur
        }
    }

    #[inline]
    pub fn push_push(&mut self, value: Value, span: Span) -> usize {
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
    pub fn fetch_u8(&self, at: usize) -> Option<u8> {
        self.ops.get(at).copied()
    }

    #[must_use]
    pub fn fetch_usize(&self, at: usize) -> Option<usize> {
        let slice = self.ops.get(at..at + 8)?;
        let bytes = slice.try_into().unwrap();
        Some(usize::from_ne_bytes(bytes))
    }

    #[must_use]
    pub fn fetch_span(&self, at: usize) -> Span {
        self.spans.get(at).copied().unwrap_or_default()
    }

    #[must_use]
    pub fn fetch_value(&self, at: usize) -> Option<Value> {
        self.vals.get(at).cloned()
    }
}
