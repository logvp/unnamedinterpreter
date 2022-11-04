use std::fmt::Display;

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
    InterpreterError(RuntimeError),
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
        Error::InterpreterError(e)
    }
}

#[derive(Debug)]
pub enum LexerError {
    Eof,
    UnknownToken(String, Loc),
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "End of file input"),
            Self::UnknownToken(snip, loc) => {
                write!(f, "Unknown Token: \"{}[...]\"\nat: {}", snip, loc)
            }
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    SyntaxError(Loc),
    Eof(Loc),
    Error(String),
    // UnbalancedParen(Loc),
}

#[derive(Debug)]
pub enum RuntimeError {
    Error(String),
}
