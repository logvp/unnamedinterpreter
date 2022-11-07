use crate::lexer;
use std::fmt::Display;

#[derive(Debug)]
pub enum AstConstruct {
    Ast,
    AstNode,
    Block,
    Statement,
    Let,
    Set,
    Expression,
    Term,
    Factor,
    Lvalue,
    Identifier,
    Literal,
    FunctionCall,
    ParameterList,
    Lambda,
}

pub use crate::interpreter::RuntimeType;

#[derive(Debug, Clone, Copy, Default)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}
impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}, col: {}", self.line, self.col)
    }
}

#[derive(Debug)]
pub enum Error {
    LexerError(LexerError),
    ParserError(ParserError),
    RuntimeError(RuntimeError),
}
impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Error::LexerError(e)
    }
}
impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Error::ParserError(e)
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Error::RuntimeError(e)
    }
}

#[derive(Debug)]
pub enum LexerError {
    Eof,
    UnknownToken(String, Loc),
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedButFound(lexer::Token, lexer::Token),
    ExpectedButFoundIn(lexer::Token, lexer::Token, AstConstruct),
    ExpectedTheseButFound(Vec<lexer::Token>, lexer::Token),
    ExpectedConstruct(AstConstruct),
    ExpectedConstructIn(AstConstruct, AstConstruct),
}

#[derive(Debug)]
pub enum RuntimeError {
    ExpectedButFound(RuntimeType, RuntimeType),
    ExpectedArgumentsFound(usize, usize),
    VariableAlreadyDeclared(String),
    UnknownVariable(String),
}
