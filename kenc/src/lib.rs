use crate::lexer::LexError;
use crate::parser::ParseError;

pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod token;

#[derive(Debug, Clone)]
pub enum CompileError {
    Lex(Vec<LexError>),
    Parse(Vec<ParseError>),
}

impl From<Vec<LexError>> for CompileError {
    fn from(value: Vec<LexError>) -> Self {
        Self::Lex(value)
    }
}

impl From<Vec<ParseError>> for CompileError {
    fn from(value: Vec<ParseError>) -> Self {
        Self::Parse(value)
    }
}
