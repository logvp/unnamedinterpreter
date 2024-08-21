use crate::ast::{BinaryOperator, Construct};
use crate::bytecode::typechecker::TypeError;
use crate::interpreter::RuntimeType;
use crate::lexer;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
    pub filename: Option<crate::String>,
}
impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.filename {
            Some(name) => write!(f, "{name}:{}:{}:", self.line, self.col),
            None => write!(f, "REPL:{}:{}:", self.line, self.col),
        }
    }
}
impl Loc {
    pub fn new(filename: Option<crate::String>) -> Self {
        Loc {
            line: 1,
            col: 1,
            filename,
        }
    }

    pub fn inc(&mut self, ch: char) {
        match ch {
            '\n' => {
                self.line += 1;
                self.col = 1;
            }
            _ => {
                self.col += 1;
            }
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Lexer(LexicalError),
    Syntax(SyntaxError),
    Type(TypeError),
    Runtime(RuntimeError),
}
impl From<LexicalError> for Error {
    fn from(e: LexicalError) -> Self {
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
impl From<TypeError> for Error {
    fn from(e: TypeError) -> Self {
        Error::Type(e)
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer(e) => write!(f, "{}", e),
            Self::Syntax(e) => write!(f, "{}", e),
            Self::Type(e) => write!(f, "{:?}", e),
            Self::Runtime(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Debug)]
pub enum LexicalError {
    UnknownToken(crate::String, Loc),
    UnterminatedStringLiteral(crate::String, Loc),
    UnmatchedMultilineComment(Loc),
    InvalidEscape(char, Loc),
    BadHexLiteral(crate::String, Loc),
    BadBinLiteral(crate::String, Loc),
}

#[derive(Debug)]
pub enum SyntaxError {
    ExpectedTokenIn(lexer::TokenKind, lexer::TokenKind, Construct, Loc),
    UnexpectedTokenIn(lexer::TokenKind, Construct, Loc),
    ExpressionMayOnlyComeAtEndIn(Construct, Loc),
    AssignmentRequiresLvalue(Loc),
    ParenthesisRequired(BinaryOperator, Construct, Loc),
}

#[derive(Debug)]
pub enum RuntimeError {
    ExpectedButFound(RuntimeType, RuntimeType),
    ExpectedArgumentsFound(usize, usize),
    VariableRedeclaration(crate::String),
    ConstReassignment(crate::String),
    UnknownIdentifier(crate::String),
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownToken(tok, loc) => write!(f, "Unknown token {} at {}", tok, loc),
            Self::UnterminatedStringLiteral(string, loc) => {
                write!(f, "Unterminated string literal \"{}\" at {}", string, loc)
            }
            Self::UnmatchedMultilineComment(loc) => {
                write!(f, "Unmatched multiline comment beginning at {}", loc)
            }
            Self::InvalidEscape(ch, loc) => {
                write!(f, "Invalid string escape sequence {:?} at {}", ch, loc)
            }
            Self::BadHexLiteral(string, loc) => {
                write!(f, "Invalid hex literal \"0x{}\" at {}", string, loc)
            }
            Self::BadBinLiteral(string, loc) => {
                write!(f, "Invalid binary literal \"0b{}\" at {}", string, loc)
            }
        }
    }
}
impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedTokenIn(expected, found, ort, loc) => {
                write!(
                    f,
                    "{loc} While parsing {ort}, expected {expected} but found {found} instead"
                )
            }
            Self::UnexpectedTokenIn(found, ort, loc) => {
                write!(
                    f,
                    "{loc} While parsing {ort}, found unexpected token {found}"
                )
            }
            Self::ExpressionMayOnlyComeAtEndIn(ort, loc) => {
                write!(f, "{loc} Expression may only be in final position of {ort}")
            }
            Self::AssignmentRequiresLvalue(loc) => {
                write!(
                    f,
                    "{loc} This expression does not resolve to a valid lvalue"
                )
            }
            Self::ParenthesisRequired(op, _ort, loc) => {
                write!(f, "{loc} Parenthesis are required around operator {op:?}",)
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
