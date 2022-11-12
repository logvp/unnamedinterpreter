use crate::error::Error;
use crate::interpreter::{Interpreter, RuntimeValue};
use std::collections::HashMap;
use std::io;
use std::io::{BufRead, Write};

pub struct Repl<I: BufRead, O: Write> {
    interpreter: Interpreter,
    input: I,
    output: O,
    macros: HashMap<String, String>,
    name: String,
    macro_buffer: String,
    multiline_buffer: String,
    multiline: bool,
    recording: bool,
}
impl<I: BufRead, O: Write> Repl<I, O> {
    pub fn new(input: I, output: O) -> Self {
        Repl {
            interpreter: Interpreter::new(),
            input: input,
            output: output,
            macros: HashMap::new(),
            name: String::new(),
            macro_buffer: String::new(),
            multiline_buffer: String::new(),
            multiline: false,
            recording: false,
        }
    }

    pub fn start(&mut self) -> io::Result<()> {
        while self.repl_line()? {}
        Ok(())
    }

    fn repl_command(&mut self, line: &str) -> io::Result<bool> {
        fn repl_help<O: Write>(output: &mut O) -> io::Result<()> {
            writeln!(
                output,
"REPL: REPL help; For language help type `.HELP`
    REPL Commands:
        HELP - Displays language help
        REPL - Displays this help message
        QUIT - Closes the REPL
        BEGIN [identifier] - Records any subsequent commands as a REPL macro. Stops recording and saves when END is entered
        END - Stops recording and saves a REPL macro
        PRINT [identifier] - Prints the contents of a REPL macro
        PLAY [identifier] - Feeds a REPL macro into the interpreter"
            )?;
            Ok(())
        }
        fn help<O: Write>(output: &mut O) -> io::Result<()> {
            writeln!(
                output,
                "REPL: Language help; For REPL help type `.REPL`
    unnamedinterpreter
        TODO: help page"
            )?;
            Ok(())
        }

        // writeln!(self.output, "HELLO THIS IS THE REPL SPEAKING")?;
        let mut words = line.split_whitespace();
        match words.next().unwrap() {
            ".QUIT" => return Ok(false),
            ".HELP" => help(&mut self.output)?,
            ".REPL" => repl_help(&mut self.output)?,
            ".BEGIN" => self.start_recording(words.next().unwrap().to_string()),
            ".END" => self.stop_recording(),
            ".PRINT" => writeln!(
                self.output,
                "REPL: \n> {:?}",
                self.macros.get(words.next().unwrap())
            )?,
            ".PLAY" => self.play_macro(words.next().unwrap().to_string())?,
            ".{" => self.start_buffering(),
            ".}" => self.send_buffer()?,
            "." => writeln!(self.output, "REPL: Type `.HELP` for help")?,
            _ => writeln!(
                self.output,
                "REPL: Unknown command.\nREPL: Type `.HELP` or `.REPL` for help"
            )?,
        }
        Ok(true)
    }
    fn start_recording(&mut self, name: String) {
        self.macro_buffer.clear();
        self.name = name;
        self.recording = true;
    }
    fn stop_recording(&mut self) {
        self.recording = false;
        self.macros
            .insert(self.name.clone(), self.macro_buffer.clone());
        self.macro_buffer.clear();
        self.name.clear();
    }
    fn play_macro(&mut self, name: String) -> io::Result<()> {
        let playback = self.macros.get(&name).unwrap();
        let result = self.interpreter.interpret(playback.to_owned());
        self.print_results(result)?;
        Ok(())
    }

    fn start_buffering(&mut self) {
        self.multiline_buffer.clear();
        self.multiline = true;
    }
    fn send_buffer(&mut self) -> io::Result<()> {
        if !self.multiline {
            writeln!(self.output, "REPL: `.{{` must come before `.}}`")?;
            return Ok(());
        }
        let result = self.interpreter.interpret(self.multiline_buffer.clone());
        self.print_results(result)?;
        self.multiline = false;
        self.multiline_buffer.clear();
        Ok(())
    }
    fn repl_line(&mut self) -> io::Result<bool> {
        let mut buffer = String::new();
        if self.multiline {
            write!(self.output, r"\ ")?;
        } else {
            write!(self.output, r"> ")?;
        }
        self.output.flush()?;
        self.input.read_line(&mut buffer)?;
        if buffer.starts_with('.') {
            return self.repl_command(&buffer);
        } else if self.multiline {
            self.multiline_buffer += &buffer;
        } else {
            if self.recording {
                self.macro_buffer.push_str(&buffer);
            }
            let result = self.interpreter.interpret(buffer);
            self.print_results(result)?;
        }
        Ok(true)
    }

    fn print_results(&mut self, result: Vec<Result<RuntimeValue, Error>>) -> io::Result<()> {
        for ret in result {
            match ret {
                Ok(RuntimeValue::None) => (),
                Ok(v) => writeln!(self.output, ": {}", v)?,
                Err(e) => writeln!(self.output, ": ERROR : {}", e)?,
            }
        }
        Ok(())
    }
}

pub fn init() -> io::Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout();

    let mut repl = Repl::new(stdin, stdout);
    repl.start()?;
    Ok(())
}
