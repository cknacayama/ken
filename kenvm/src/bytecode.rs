use std::fmt::Display;

use kenspan::Span;

use crate::value::Value;

#[derive(Clone, Copy)]
#[repr(u8)]
enum OpCode {
    Nop,

    Pop,
    Push,

    PushBool,
    PushU32,

    MakeList,
    MakeTuple,
    MakeTable,

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

    Call,
    CallMethod,

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
    #[inline]
    const fn decode(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Self::Nop),
            1 => Some(Self::Pop),
            2 => Some(Self::Push),
            3 => Some(Self::PushBool),
            4 => Some(Self::PushU32),
            5 => Some(Self::MakeList),
            6 => Some(Self::MakeTuple),
            7 => Some(Self::MakeTable),
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
            19 => Some(Self::Call),
            20 => Some(Self::CallMethod),
            21 => Some(Self::AddLocal),
            22 => Some(Self::AddGlobal),
            23 => Some(Self::LoadIdx),
            24 => Some(Self::StoreIdx),
            25 => Some(Self::Load),
            26 => Some(Self::Store),
            27 => Some(Self::Restore),
            28 => Some(Self::LoadGlobal),
            29 => Some(Self::StoreGlobal),
            30 => Some(Self::Jmp),
            31 => Some(Self::JmpIfNot),
            32 => Some(Self::Ret),
            _ => None,
        }
    }
}

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
            Self::PushBool(_) => OpCode::PushBool,
            Self::PushU32(_) => OpCode::PushU32,
            Self::MakeList(_) => OpCode::MakeList,
            Self::MakeTuple(_) => OpCode::MakeTuple,
            Self::MakeTable(_) => OpCode::MakeTable,
            Self::Neg => OpCode::Neg,
            Self::Not => OpCode::Not,
            Self::Add => OpCode::Add,
            Self::Sub => OpCode::Sub,
            Self::Mul => OpCode::Mul,
            Self::Div => OpCode::Div,
            Self::Rem => OpCode::Rem,
            Self::Pow => OpCode::Pow,
            Self::Eq => OpCode::Eq,
            Self::Lt => OpCode::Lt,
            Self::Le => OpCode::Le,
            Self::Call(_) => OpCode::Call,
            Self::CallMethod(_, _) => OpCode::CallMethod,
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

    fn encode_args(&self) -> ChunkBuffer<9> {
        let mut chunk = ChunkBuffer::new();
        match *self {
            Self::PushBool(b) => {
                chunk.push(u8::from(b));
            }
            Self::Call(arg) => {
                chunk.push(arg);
            }
            Self::CallMethod(arg, name) => {
                chunk.push(arg);
                for b in name.to_ne_bytes() {
                    chunk.push(b);
                }
            }
            Self::PushU32(arg) => {
                for b in arg.to_ne_bytes() {
                    chunk.push(b);
                }
            }
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
            | Self::JmpIfNot(arg) => {
                for b in arg.to_ne_bytes() {
                    chunk.push(b);
                }
            }
            _ => {}
        }
        chunk
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
        let at = at + 1;
        if matches!(code, OpCode::Jmp | OpCode::JmpIfNot) {
            let slice = self.ops.get_mut(at..at + size_of::<usize>())?;
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

    pub fn push_value(&mut self, value: Value) -> usize {
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

impl Fetch<u8> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<u8> {
        let byte = self.chunk.ops.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }
}

impl Fetch<bool> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<bool> {
        let byte = self.chunk.ops.get(self.ip).copied()?;
        let b = match byte {
            0 => false,
            1 => true,
            _ => return None,
        };
        self.ip += 1;
        Some(b)
    }
}

impl Fetch<u16> for OpStream<'_> {
    fn fetch(&mut self) -> Option<u16> {
        let at = self.ip;
        let slice = self.chunk.ops.get(at..at + size_of::<u16>())?;
        let bytes = slice.try_into().unwrap();
        let word = u16::from_ne_bytes(bytes);
        self.ip += size_of::<u16>();
        Some(word)
    }
}

impl Fetch<u32> for OpStream<'_> {
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
        self.fetch().and_then(OpCode::decode)
    }
}

impl Fetch<Span> for OpStream<'_> {
    fn fetch(&mut self) -> Option<Span> {
        self.chunk.fetch_span(self.ip - 1)
    }
}

impl<T, U> Fetch<(T, U)> for OpStream<'_>
where
    Self: Fetch<T> + Fetch<U>,
{
    fn fetch(&mut self) -> Option<(T, U)> {
        let t = self.fetch()?;
        let u = self.fetch()?;
        Some((t, u))
    }
}

impl Fetch<Op> for OpStream<'_> {
    #[inline]
    fn fetch(&mut self) -> Option<Op> {
        let op = self.fetch()?;
        match op {
            OpCode::Nop => Some(Op::Nop),
            OpCode::Pop => Some(Op::Pop),
            OpCode::Push => self.fetch().map(Op::Push),
            OpCode::PushBool => self.fetch().map(Op::PushBool),
            OpCode::PushU32 => self.fetch().map(Op::PushU32),
            OpCode::MakeList => self.fetch().map(Op::MakeList),
            OpCode::MakeTuple => self.fetch().map(Op::MakeTuple),
            OpCode::MakeTable => self.fetch().map(Op::MakeTable),
            OpCode::Neg => Some(Op::Neg),
            OpCode::Not => Some(Op::Not),
            OpCode::Add => Some(Op::Add),
            OpCode::Sub => Some(Op::Sub),
            OpCode::Mul => Some(Op::Mul),
            OpCode::Div => Some(Op::Div),
            OpCode::Rem => Some(Op::Rem),
            OpCode::Pow => Some(Op::Pow),
            OpCode::Eq => Some(Op::Eq),
            OpCode::Lt => Some(Op::Lt),
            OpCode::Le => Some(Op::Le),
            OpCode::Call => self.fetch().map(Op::Call),
            OpCode::CallMethod => self.fetch().map(|(t, u)| Op::CallMethod(t, u)),
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
            Self::AddLocal => write!(f, "add local"),
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
        writeln!(f, "data:")?;
        let max = self.vals.len().ilog10() as usize + 1;
        for (at, value) in self.vals.iter().enumerate() {
            writeln!(f, "    {at:>max$}:    {value}")?;
        }
        Ok(())
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
