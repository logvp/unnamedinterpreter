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
        repl::run_file(&path)
    } else {
        repl::init()
    }
}

#[cfg(test)]
mod tests {
    use crate::interpreter::Interpreter;
    use crate::lexer::Lexer;
    use crate::lexer::Token;
    use crate::parser::Parser;
    use crate::repl;

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

    impl Lexer {
        pub fn get_all_tokens(&mut self) -> Vec<Token> {
            let mut tokens: Vec<Token> = Default::default();
            loop {
                match self.next_token() {
                    Token::Eof => break,
                    token => tokens.push(token),
                }
            }
            tokens
        }
    }

    #[test]
    fn lexer() {
        let mut lexer = Lexer::lex(PROGRAM).unwrap();
        println!("--- Tokens ---\n{:?}", lexer.get_all_tokens());
    }

    #[test]
    fn parser() {
        let mut parser = Parser::new(PROGRAM).unwrap();
        let ast = parser.gen_ast().unwrap();
        println!("--- AST ---\n{}", ast);
    }

    #[test]
    fn interpreter() {
        let mut interpreter = Interpreter::new();
        let ret = interpreter.interpret(PROGRAM);
        println!("--- Results ---");
        for v in ret {
            println!("OK: {:?}", v.unwrap())
        }
    }

    #[test]
    fn examples() {
        use std::fs;

        for file in fs::read_dir("./examples").unwrap() {
            repl::run_file(&file.unwrap().path()).unwrap();
        }
    }
}
