mod ast;
mod bytecode;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod string;
mod treewalk;
mod visitor;

use clap::{Parser, ValueEnum};

use std::io;

use bytecode::interpreter::BytecodeInterpreter;
use interpreter::Interpreter;
use treewalk::TreeWalkInterpreter;

pub use string::String;

#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Backend {
    #[default]
    Treewalk,
    Bytecode,
}

#[derive(Parser)]
#[command(version, about)]
struct Options {
    filename: Option<std::string::String>,
    #[arg(value_enum, short, long)]
    backend: Option<Backend>,
}

fn run_main<I: Interpreter>(filename: Option<std::string::String>) -> io::Result<()> {
    if let Some(path) = filename {
        repl::run_and_print_file::<I, _>(&path)
    } else {
        repl::init::<I>()
    }
}

fn main() -> io::Result<()> {
    let opts = Options::parse();

    match opts.backend.unwrap_or_default() {
        Backend::Bytecode => run_main::<BytecodeInterpreter>(opts.filename),
        Backend::Treewalk => run_main::<TreeWalkInterpreter>(opts.filename),
    }
}

#[cfg(test)]
type InterpreterImpl = BytecodeInterpreter;

#[cfg(test)]
const PROGRAM: &str = r#"
var x = 15;
set x = lambda () { 
    return := lambda (z) { 
        if (z > 0) { 
            return := "TRUE"; 
        } else { 
            return := "FALSE"; 
        }; 
        return
    };
    return
}();
v := x(99);
let y = x;
print(y(-10));
"#;

#[test]
fn lexer() {
    let _ = lexer::Lexer::lex(PROGRAM, None).unwrap();
}

#[test]
fn parser() {
    let _ = parser::Parser::gen_ast(PROGRAM, None).unwrap();
}

#[test]
fn interpreter() {
    for result in InterpreterImpl::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn examples() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        for result in repl::run_file::<InterpreterImpl, _>(&file.unwrap().path()).unwrap() {
            result.unwrap();
        }
    }
}

#[test]
fn should_error() {
    use std::fs;

    for path in ["./err/lexical", "./err/runtime"] {
        'file_loop: for file in fs::read_dir(path).unwrap() {
            for result in
                repl::run_file::<InterpreterImpl, _>(&file.as_ref().unwrap().path()).unwrap()
            {
                if result.is_err() {
                    break 'file_loop;
                }
            }
            panic!("{:?} No errors present!", &file);
        }
    }
}
