mod ast;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;

use std::io;

fn main() -> io::Result<()> {
    repl::init()
}

#[cfg(test)]
mod tests {
    const PROGRAM: &str = r#"
    1 +

    2;
    let x = 14;
    set x = 19;
    "#;

    #[test]
    fn lexer() {
        use crate::lexer::Lexer;
        let mut lexer = Lexer::new(PROGRAM.to_owned());
        println!("--- Tokens ---");
        println!("{:?}", lexer.get_all_tokens().unwrap());
    }

    #[test]
    fn parser() {
        use crate::parser::Parser;
        let mut parser = Parser::new(PROGRAM.to_owned());
        let ast = parser.gen_ast().unwrap();
        println!("--- AST ---");
        println!("{}", ast);
    }

    #[test]
    fn interpreter() {
        use crate::interpreter::Interpreter;
        let mut interpreter = Interpreter::new();
        let ret = interpreter.interpret(PROGRAM.to_owned());
        println!("--- Results ---");
        for v in ret {
            match v {
                Ok(val) => println!("{:?}", val),
                Err(e) => panic!("{:?}", e),
            }
        }
    }
}
