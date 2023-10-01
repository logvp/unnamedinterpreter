use crate::ast::Construct;
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
    Lexer(LexerError),
    Syntax(SyntaxError),
    Runtime(RuntimeError),
}
impl From<LexerError> for Error {
    fn from(e: LexerError) -> Self {
        Error::Lexer(e)
    }
}
impl From<SyntaxError> for Error {
    fn from(e: SyntaxError) -> Self {
        Error::Syntax(e)
    }
}
impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Error::Runtime(e)
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer(e) => write!(f, "{}", e),
            Self::Syntax(e) => write!(f, "{}", e),
            Self::Runtime(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    UnknownToken(String, Loc),
    UnterminatedStringLiteral(String, Loc),
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedTokenIn(lexer::Token, lexer::Token, Construct),
    UnexpectedTokenIn(lexer::Token, Construct),
    ExpressionMayOnlyComeAtEndIn(Construct),
}

#[derive(Debug)]
pub enum RuntimeError {
    ExpectedButFound(RuntimeType, RuntimeType),
    ExpectedArgumentsFound(usize, usize),
    VariableRedeclaration(String),
    ConstReassignment(String),
    UnknownIdentifier(String),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(tok, loc) => write!(f, "Unknown token {} at {}", tok, loc),
            Self::UnterminatedStringLiteral(string, loc) => {
                write!(f, "Unterminated string literal \"{}\" at {}", string, loc)
            }
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedTokenIn(expected, found, ort) => {
                write!(
                    f,
                    "While parsing {ort}, expected {expected} but found {found} instead"
                )
            }
            Self::UnexpectedTokenIn(found, ort) => {
                write!(f, "While parsing {ort}, found unexpected token {found}")
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
            Self::VariableRedeclaration(name) => {
                write!(
                    f,
                    "Identifier `{name}` has already been declared in current scope"
                )
            }
            Self::ConstReassignment(name) => {
                write!(
                    f,
                    "Identifier `{name}` was declared immutable and thus cannot be reassigned",
                )
            }
        }
    }
}
