use crate::interpreter::Interpreter;
use std::collections::HashMap;
use std::io;
use std::io::{BufRead, Write};

pub struct Repl<I: BufRead, O: Write> {
    interpreter: Interpreter,
    input: I,
    output: O,
    macros: HashMap<String, String>,
    name: String,
    buffer: String,
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
            buffer: String::new(),
            recording: false,
        }
    }

    pub fn start(&mut self) -> io::Result<()> {
        while self.repl_line()? {}
        Ok(())
    }

    fn repl_command(&mut self, line: &String) -> io::Result<bool> {
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
            "." => writeln!(self.output, "REPL: Type `.HELP` for help")?,
            _ => writeln!(
                self.output,
                "REPL: Unknown command.\nREPL: Type `.HELP` or `.REPL` for help"
            )?,
        }
        Ok(true)
    }
    fn start_recording(&mut self, name: String) {
        self.buffer.clear();
        self.name = name.to_string();
        self.recording = true;
    }
    fn stop_recording(&mut self) {
        self.recording = false;
        self.macros.insert(self.name.clone(), self.buffer.clone());
        self.buffer.clear();
        self.name.clear();
    }
    fn play_macro(&mut self, name: String) -> io::Result<()> {
        let playback = self.macros.get(&name).unwrap();
        for line in playback.lines() {
            if self.recording {
                self.buffer.push_str(&line);
            }
            let result = self.interpreter.interpret(line.to_owned());

            for ret in result {
                match ret {
                    Ok(v) => writeln!(self.output, ": {:?}", v)?,
                    Err(e) => writeln!(self.output, ": ERROR : {:?}", e)?,
                }
            }
        }
        Ok(())
    }

    fn repl_line(&mut self) -> io::Result<bool> {
        let mut buffer = String::new();
        write!(self.output, "> ")?;
        self.output.flush()?;
        self.input.read_line(&mut buffer)?;
        if buffer.starts_with('.') {
            return self.repl_command(&buffer);
        } else {
            if self.recording {
                self.buffer.push_str(&buffer);
            }
            let result = self.interpreter.interpret(buffer);

            for ret in result {
                match ret {
                    Ok(v) => writeln!(self.output, ": {:?}", v)?,
                    Err(e) => writeln!(self.output, ": ERROR : {:?}", e)?,
                }
            }
        }
        Ok(true)
    }
}

pub fn init() -> io::Result<()> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout();

    let mut repl = Repl::new(stdin, stdout);
    repl.start()?;
    Ok(())
}
