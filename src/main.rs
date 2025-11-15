use unnamedinterpreter as ui;

use std::{env, io};

fn main() -> io::Result<()> {
    let filename = env::args().nth(1);

    if let Some(path) = filename {
        ui::repl::run_and_print_file::<ui::InterpreterImpl, _>(&path)
    } else {
        ui::repl::init::<ui::InterpreterImpl>()
    }
}
