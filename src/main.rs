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

pub use string::{String, Symbol};

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
fn interpreter_treewalk() {
    for result in TreeWalkInterpreter::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn interpreter_bytecode() {
    for result in BytecodeInterpreter::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn examples_treewalk() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        let path = &file.unwrap().path();
        repl::run_file::<TreeWalkInterpreter, _>(&path)
            .unwrap()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
    }
}

#[test]
fn examples_bytecode() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        let path = &file.unwrap().path();
        repl::run_file::<BytecodeInterpreter, _>(&path)
            .unwrap()
            .into_iter()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
    }
}

#[cfg(test)]
fn dir_should_error<I: Interpreter>(path: &str) {
    use std::fs;

    'file_loop: for file in fs::read_dir(path).unwrap() {
        for result in repl::run_file::<I, _>(&file.as_ref().unwrap().path()).unwrap() {
            if result.is_err() {
                continue 'file_loop;
            }
        }
        panic!("{:?} Error was expected but none found!", &file);
    }
}

#[test]
fn should_error_lexical() {
    dir_should_error::<InterpreterImpl>("./err/lexical")
}

#[test]
fn should_error_runtime_treewalk() {
    dir_should_error::<TreeWalkInterpreter>("./err/runtime")
}

#[test]
fn should_error_runtime_bytecode() {
    dir_should_error::<BytecodeInterpreter>("./err/runtime")
}
