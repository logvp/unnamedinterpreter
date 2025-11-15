mod ast;
pub mod bytecode;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod treewalk;

pub type InterpreterImpl = treewalk::TreeWalkInterpreter;