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
    x := 15;
    if (x > 10) { print("success!"); } else { print("failure"); };
    "#;

    #[test]
    fn lexer() {
        use crate::lexer::Lexer;
        let mut lexer = Lexer::new(PROGRAM.to_owned());
        println!("--- Tokens ---\n{:?}", lexer.get_all_tokens().unwrap());
    }

    #[test]
    fn parser() {
        use crate::parser::Parser;
        let mut parser = Parser::new(PROGRAM.to_owned());
        let ast = parser.gen_ast().unwrap();
        println!("--- AST ---\n{}", ast);
    }

    #[test]
    fn interpreter() {
        use crate::interpreter::Interpreter;
        let mut interpreter = Interpreter::new();
        let ret = interpreter.interpret(PROGRAM.to_owned());
        println!("--- Results ---");
        for v in ret {
            println!("OK: {:?}", v.unwrap())
        }
    }
}
