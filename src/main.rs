mod ast;
mod bytecode;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod printer;
mod repl;
mod string;
mod treewalk;
mod visitor;

use std::io;

use bytecode::interpreter::BytecodeInterpreter;
use interpreter::Interpreter;
use treewalk::TreeWalkInterpreter;

pub use crate::string::{String, Symbol};

use clap::Parser;

// Overriding String causes errors with some of of the derive attributes
// Using an inner module prevents this
mod argp {
    use clap::{Parser, ValueEnum};

    #[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
    pub enum Backend {
        #[default]
        Treewalk,
        Bytecode,
    }

    #[derive(Parser, Default)]
    #[command(version, about)]
    pub struct Options {
        pub filename: Option<std::string::String>,
        #[arg(value_enum, short, long)]
        pub backend: Option<Backend>,
        #[arg(long, default_value_t = false)]
        pub unify_branches: bool,
        #[arg(long, default_value_t = false)]
        pub print_only: bool,
    }
}

fn run_main<I: Interpreter>(options: argp::Options) -> io::Result<()> {
    if options.filename.is_some() {
        repl::run_and_print_file::<I>(options)
    } else {
        repl::init::<I>(options)
    }
}

fn main() -> io::Result<()> {
    let opts = argp::Options::parse();

    if opts.print_only {
        let ast = crate::parser::Parser::gen_ast(
            std::fs::read_to_string(opts.filename.unwrap())
                .unwrap()
                .as_ref(),
            None,
        )
        .unwrap();
        println!("{}", printer::Printer::print(&ast));
        return Ok(());
    }

    match opts.backend.unwrap_or_default() {
        argp::Backend::Bytecode => run_main::<BytecodeInterpreter>(opts),
        argp::Backend::Treewalk => run_main::<TreeWalkInterpreter>(opts),
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
    for result in TreeWalkInterpreter::new(argp::Options::default()).interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn interpreter_bytecode() {
    for result in BytecodeInterpreter::new(argp::Options::default()).interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn examples_treewalk() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        let path = &file.unwrap().path();
        repl::run_file::<TreeWalkInterpreter, _>(&path, argp::Options::default())
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
        repl::run_file::<BytecodeInterpreter, _>(&path, argp::Options::default())
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
        for result in
            repl::run_file::<I, _>(&file.as_ref().unwrap().path(), argp::Options::default())
                .unwrap()
        {
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

#[test]
fn printer_examples() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        let text = fs::read_to_string(file.as_ref().unwrap().path()).unwrap();
        let ast = parser::Parser::gen_ast(&text, None).unwrap();
        let gen_text = printer::Printer::print(&ast);
        let check_ast = parser::Parser::gen_ast(&gen_text, None).unwrap();
        assert_eq!(ast, check_ast);
    }
}
