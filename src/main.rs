mod ast;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;

use std::io;

fn main() -> io::Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout();

    let mut repl = repl::Repl::new(stdin, stdout);
    repl.start()?;

    Ok(())
}
