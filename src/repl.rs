use crate::error::Error;
use crate::interpreter::Interpreter;
use std::collections::HashMap;
use std::io;
use std::io::{BufRead, Write};

pub struct Repl<T: Interpreter, I: BufRead, O: Write> {
    interpreter: T,
    input: I,
    output: O,
    macros: HashMap<String, String>,
    name: String,
    macro_buffer: String,
    multiline_buffer: String,
    multiline: bool,
    recording: bool,
}
impl<T: Interpreter, I: BufRead, O: Write> Repl<T, I, O> {
    pub fn new(input: I, output: O) -> Self {
        Repl {
            interpreter: T::new(),
            input,
            output,
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
"REPL: REPL help; For language help type `:HELP`
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
                "REPL: Language help; For REPL help type `:REPL`
    unnamedinterpreter
        TODO: help page"
            )?;
            Ok(())
        }

        let mut words = {
            let mut chars = line.chars();
            chars.next();
            chars.as_str()
        }
        .split_whitespace();
        match words.next() {
            Some("QUIT") => return Ok(false),
            Some("HELP") => help(&mut self.output)?,
            Some("REPL") => repl_help(&mut self.output)?,
            Some("BEGIN") => self.start_recording(words.next().unwrap().to_string()),
            Some("END") => self.stop_recording(),
            Some("PRINT") => writeln!(
                self.output,
                "REPL: \n> {:?}",
                self.macros.get(words.next().unwrap())
            )?,
            Some("PLAY") => self.play_macro(words.next().unwrap().to_string())?,
            Some("{") => self.start_buffering(),
            Some("}") => self.send_buffer()?,
            _ => writeln!(
                self.output,
                "REPL: Unknown command.\nREPL: Type `:HELP` or `:REPL` for help"
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
        let result = self.interpreter.interpret(playback, None);
        Self::print_results(&mut self.output, &result)?;
        Ok(())
    }

    fn start_buffering(&mut self) {
        self.multiline_buffer.clear();
        self.multiline = true;
    }
    fn send_buffer(&mut self) -> io::Result<()> {
        if !self.multiline {
            writeln!(self.output, "REPL: `:{{` must come before `:}}`")?;
            return Ok(());
        }
        let result = self.interpreter.interpret(&self.multiline_buffer, None);
        Self::print_results(&mut self.output, &result)?;
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
        if buffer.starts_with(':') {
            return self.repl_command(&buffer);
        } else if self.multiline {
            self.multiline_buffer += &buffer;
        } else {
            if self.recording {
                self.macro_buffer.push_str(&buffer);
            }
            let result = self.interpreter.interpret(&buffer, None);
            Self::print_results(&mut self.output, &result)?;
        }
        Ok(true)
    }

    fn print_results(output: &mut O, result: &[Result<T::ReplReturn, Error>]) -> io::Result<()> {
        for ret in result {
            match ret {
                Ok(v) => writeln!(output, ": {}", v)?,
                Err(e) => writeln!(output, ": ERROR : {}", e)?,
            }
        }
        Ok(())
    }
}

pub fn init<T: Interpreter>() -> io::Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout();

    let mut repl = Repl::<T, _, _>::new(stdin, stdout);
    repl.start()?;
    Ok(())
}

pub fn run_file<T: Interpreter, P: AsRef<std::path::Path>>(
    path: &P,
) -> io::Result<Vec<Result<T::ReplReturn, Error>>> {
    let content = std::fs::read_to_string(path)?;
    Ok(T::new().interpret(&content, path.as_ref().to_str().map(|name| name.into())))
}

pub fn run_and_print_file<T: Interpreter, P: AsRef<std::path::Path>>(path: &P) -> io::Result<()> {
    if let Ok(result) = run_file::<T, P>(path) {
        Repl::<T, io::StdinLock, _>::print_results(&mut io::stdout(), &result)
    } else {
        eprintln!(
            "Could not open file '{}'",
            path.as_ref()
                .file_name()
                .expect("Couldn't open file and can't display file name")
                .to_string_lossy()
        );
        Ok(())
    }
}
