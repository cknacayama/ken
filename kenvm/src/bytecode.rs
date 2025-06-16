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
            10 => Some(Self::Rem),
            11 => Some(Self::Eq),
            12 => Some(Self::Lt),
            13 => Some(Self::Le),
            14 => Some(Self::Call),
            15 => Some(Self::AddLocal),
            16 => Some(Self::AddGlobal),
            17 => Some(Self::Load),
            18 => Some(Self::Store),
            19 => Some(Self::Restore),
            20 => Some(Self::LoadGlobal),
            21 => Some(Self::StoreGlobal),
            22 => Some(Self::Jmp),
            23 => Some(Self::JmpUnless),
            24 => Some(Self::Ret),
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
    /// No operation.
    Nop,

    /// Pop [Value] from stack.
    Pop,

    /// Push [Value] into stack.
    Push(usize),

    /// Push `u32` into stack.
    PushInt(u32),

    /// Pop [Value] from stack
    /// and try to negate it.
    ///
    /// The result is pushed back
    /// into  the stack.
    Neg,

    /// Pop [Value] from stack
    /// and try to 'not' it.
    ///
    /// The result is pushed back
    /// into  the stack.
    Not,

    /// Pop two [Value]'s from stack
    /// and try to add them.
    ///
    /// The result is pushed back
    /// into  the stack.
    Add,

    /// Pop two [Value]'s from stack
    /// and try to subtract them.
    ///
    /// The result is pushed back
    /// into  the stack.
    Sub,

    /// Pop two [Value]'s from stack
    /// and try to multiply them.
    ///
    /// The result is pushed back
    /// into  the stack.
    Mul,

    /// Pop two [Value]'s from stack
    /// and try to divide them.
    ///
    /// The result is pushed back
    /// into  the stack.
    ///
    /// # Errors
    ///
    /// Will return `Err` if first element
    /// on the stack is `0`.
    Div,

    /// Pop two [Value]'s from stack
    /// and try to take the remainder of them.
    ///
    /// The result is pushed back
    /// into  the stack.
    ///
    /// # Errors
    ///
    /// Will return `Err` if first element
    /// on the stack is `0`.
    Rem,

    /// Pop two [Value]'s from stack
    /// and try to check equality on them.
    ///
    /// The result of type `Bool` is pushed back
    /// into  the stack.
    Eq,

    /// Pop two [Value]'s from stack
    /// and try to check if lhs is less than rhs.
    ///
    /// The result of type `Bool` is pushed back
    /// into  the stack.
    Lt,

    /// Pop two [Value]'s from stack
    /// and try to check if lhs is less than or equal to rhs.
    ///
    /// The result of type `Bool` is pushed back
    /// into  the stack.
    Le,

    /// Pop callee and
    /// arguments from stack and tries
    /// to call callee.
    ///
    /// The returned value is pushed back
    /// into  the stack.
    Call(u8),

    /// Pop [Value] from stack
    /// and push it into local variables
    /// stack.
    AddLocal,

    /// Pop [Value] from stack
    /// and push it into global variables
    /// stack.
    AddGlobal,

    /// Reads a [Value] from local variables stack and
    /// pushes it into the stack.
    Load(usize),

    /// Pop [Value] from stack
    /// and store it into local variables
    /// stack at a certain position.
    Store(usize),

    /// Pop n [Value]'s from local
    /// variables stack.
    Restore(usize),

    /// Reads a [Value] from global variables stack and
    /// pushes it into the stack.
    LoadGlobal(usize),

    /// Pop [Value] from stack
    /// and store it into global variables
    /// stack at a certain position.
    StoreGlobal(usize),

    /// Jump to instruction.
    Jmp(usize),

    /// Pop [Value] from stack
    /// and jump to instruction
    /// if the `Value` is `true`.
    JmpUnless(usize),

    /// Pop [Value] from stack
    /// and returns it.
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
            Self::Rem => OpCode::Rem,
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
            Self::Rem => write!(f, "rem"),
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
