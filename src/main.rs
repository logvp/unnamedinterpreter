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
    use crate::error::*;
    use crate::interpreter::Interpreter;
    use crate::lexer::Lexer;
    use crate::lexer::Token;
    use crate::parser::Parser;

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
        pub fn get_all_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
            let mut tokens: Vec<Token> = Default::default();
            while self.has_next() {
                match self.next_token() {
                    Ok(Token::Eof) => break,
                    Ok(token) => tokens.push(token),
                    Err(error) => return Err(error),
                }
            }
            Ok(tokens)
        }
    }

    #[test]
    fn lexer() {
        let mut lexer = Lexer::new(PROGRAM.to_owned());
        println!("--- Tokens ---\n{:?}", lexer.get_all_tokens().unwrap());
    }

    #[test]
    fn parser() {
        let mut parser = Parser::new(PROGRAM.to_owned());
        let ast = parser.gen_ast().unwrap();
        println!("--- AST ---\n{}", ast);
    }

    #[test]
    fn interpreter() {
        let mut interpreter = Interpreter::new();
        let ret = interpreter.interpret(PROGRAM.to_owned());
        println!("--- Results ---");
        for v in ret {
            println!("OK: {:?}", v.unwrap())
        }
    }

    fn run_file<P: AsRef<std::path::Path>>(path: &P) {
        if let Ok(content) = std::fs::read_to_string(path) {
            let result = Interpreter::new().interpret(content);
            for it in result {
                println!("{}", it.unwrap());
            }
        } else {
            println!(
                "Could not open file '{}'",
                path.as_ref()
                    .file_name()
                    .expect("Couldn't open file and can't display file name")
                    .to_string_lossy()
            );
        }
    }

    #[test]
    fn examples() {
        use std::fs;

        for file in fs::read_dir("./examples").unwrap() {
            run_file(&file.unwrap().path());
        }
    }
}
