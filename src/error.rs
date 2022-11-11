use crate::ast::ConstructKind;
use crate::lexer;
use std::fmt::Display;

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
    SyntaxError(SyntaxError),
    RuntimeError(RuntimeError),
}
impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Error::LexerError(e)
    }
}
impl From<SyntaxError> for Error {
    fn from(e: SyntaxError) -> Self {
        Error::SyntaxError(e)
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Error::RuntimeError(e)
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexerError(e) => write!(f, "{}", e),
            Self::SyntaxError(e) => write!(f, "{}", e),
            Self::RuntimeError(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    // Eof,
    UnknownToken(String, Loc),
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedToken(lexer::Token, lexer::Token),
    ExpectedTokenIn(lexer::Token, lexer::Token, ConstructKind),
    ExpectedTheseButFound(Vec<lexer::Token>, lexer::Token),
    ExpectedConstruct(ConstructKind),
    ExpectedConstructIn(ConstructKind, ConstructKind),
    UnexpectedToken(lexer::Token),
    ExpressionMayOnlyComeAtEndIn(ConstructKind),
}

#[derive(Debug)]
pub enum RuntimeError {
    ExpectedButFound(RuntimeType, RuntimeType),
    ExpectedArgumentsFound(usize, usize),
    VariableAlreadyDeclared(String),
    UnknownIdentifier(String),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(tok, loc) => write!(f, "Unknown token {} at {}", tok, loc),
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedToken(expected, found) => {
                write!(f, "Expected {expected} but found {found} instead")
            }
            Self::ExpectedTokenIn(expected, found, ort) => {
                write!(
                    f,
                    "While parsing {ort}, expected {expected} but found {found} instead"
                )
            }
            Self::ExpectedTheseButFound(expected_list, found) => {
                write!(f, "Expected ")?;
                let mut iter = expected_list.into_iter();
                if let Some(x) = iter.next() {
                    write!(f, "{}", x)?;
                }
                for x in iter {
                    write!(f, " or {}", x)?;
                }
                write!(f, ", but found {found} instead")
            }
            Self::ExpectedConstruct(expected) => {
                write!(f, "Expected {expected}")
            }
            Self::ExpectedConstructIn(expected, ort) => {
                write!(f, "While parsing {ort} expected {expected}")
            }
            Self::UnexpectedToken(found) => {
                write!(f, "Unexpected token {found}")
            }
            Self::ExpressionMayOnlyComeAtEndIn(ort) => {
                write!(f, "Expression may only be in final position of {ort}")
            }
        }
    }
}
impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedArgumentsFound(expected, found) => write!(
                f,
                "Function expected {expected} arguments but {found} were provided"
            ),
            Self::ExpectedButFound(expected, found) => {
                write!(f, "Expected type {expected} but found {found} instead")
            }
            Self::UnknownIdentifier(name) => {
                write!(f, "Identifier `{name}` has not been defined in this scope")
            }
            Self::VariableAlreadyDeclared(name) => {
                write!(
                    f,
                    "Identifier `{name}` has already been declared in current scope"
                )
            }
        }
    }
}
