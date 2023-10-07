mod ast;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;

use std::{env, io};

fn main() -> io::Result<()> {
    let filename = env::args().nth(1);

    if let Some(path) = filename {
        repl::run_and_print_file(&path)
    } else {
        repl::init()
    }
}

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
    let mut parser = parser::Parser::new(PROGRAM, None).unwrap();
    let _ = parser.gen_ast().unwrap();
}

#[test]
fn interpreter() {
    for result in interpreter::Interpreter::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn examples() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        for result in repl::run_file(&file.unwrap().path()).unwrap() {
            result.unwrap();
        }
    }
}

#[test]
fn should_error() {
    use std::fs;

    for path in ["./err/lexical", "./err/runtime"] {
        'file_loop: for file in fs::read_dir(path).unwrap() {
            for result in repl::run_file(&file.as_ref().unwrap().path()).unwrap() {
                if result.is_err() {
                    break 'file_loop;
                }
            }
            panic!("{:?} No errors present!", &file);
        }
    }
}
