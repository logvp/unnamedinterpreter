use unnamedinterpreter as ui;
use ui::interpreter::Interpreter;

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
    let _ = ui::lexer::Lexer::lex(PROGRAM, None).unwrap();
}

#[test]
fn parser() {
    let _ = ui::parser::Parser::gen_ast(PROGRAM, None).unwrap();
}

#[test]
fn treewalk_interpreter() {
    for result in ui::treewalk::TreeWalkInterpreter::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn bytecode_interpreter() {
    for result in ui::bytecode::interpreter::BytecodeInterpreter::new().interpret(PROGRAM, None) {
        result.unwrap();
    }
}

#[test]
fn examples() {
    use std::fs;

    for file in fs::read_dir("./examples").unwrap() {
        for result in ui::repl::run_file::<ui::InterpreterImpl, _>(&file.unwrap().path()).unwrap() {
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
                ui::repl::run_file::<ui::InterpreterImpl, _>(&file.as_ref().unwrap().path()).unwrap()
            {
                if result.is_err() {
                    break 'file_loop;
                }
            }
            panic!("{:?} No errors present!", &file);
        }
    }
}
