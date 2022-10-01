extern crate regex;

pub use crate::ast::Literal;
use crate::error::{LexerError, Loc};
use regex::Regex;

#[derive(Debug, Clone)]
pub enum Token {
    Literal(Literal),
    LeftParen,
    RightParen,
    LeftAngle,
    RightAngle,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    ColonEqual,
    Eof,
}

const MAX_SNIPPET_LENGTH: usize = 20;

#[derive(Debug)]
pub struct Lexer {
    text: String,
    cursor: usize,
    loc: Loc,
}
impl Lexer {
    // Lexer owns text
    pub fn new(text: String) -> Self {
        Lexer {
            text,
            cursor: 0,
            loc: Default::default(),
        }
    }

    pub fn loc(&self) -> Loc {
        self.loc
    }

    pub fn has_next(&self) -> bool {
        self.cursor < self.text.len()
    }

    fn move_cursor(&mut self, update: usize) {
        self.cursor += update;
        self.loc.col += update;
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if !self.has_next() {
            if self.cursor == self.text.len() - 1 {
                self.cursor += 1;
                return Ok(Token::Eof);
            } else {
                return Err(LexerError::Eof);
            }
        }
        // text = string[cursor:]
        let text = self.text.split_at(self.cursor).1;
        if text.len() == 0 {
            return Err(LexerError::Eof);
        }
        // update loc on newline
        if let Some(mat) = find(r"^\n", text) {
            self.loc.line += 1;
            let delta = mat.end();
            self.move_cursor(delta);
            // either this or return whitespace error?
            return self.next_token();
        }
        // skip whitespace
        if let Some(mat) = find(r"^\s+", text) {
            let delta = mat.end();
            self.move_cursor(delta);
            // either this or return whitespace error?
            return self.next_token();
        }
        // grab integers
        if let Some(mat) = find(r"^\d+", text) {
            let delta = mat.end();
            let int: i32 = mat.as_str().parse().unwrap();
            self.move_cursor(delta);
            return Ok(Token::Literal(Literal::IntLiteral(int)));
        }
        // grab quotes
        if let Some(mat) = find(r#"^"[^"]*""#, text) {
            let delta = mat.end();
            let r = Token::Literal(Literal::StringLiteral(
                mat.as_str()
                    .strip_prefix('"')
                    .unwrap()
                    .strip_suffix('"')
                    .unwrap()
                    .to_owned(),
            ));
            self.move_cursor(delta);
            return Ok(r);
        }
        if let Some(mat) = find(r"^:=", text) {
            let delta = mat.end();
            self.move_cursor(delta);
            return Ok(Token::ColonEqual);
        }
        // all single characters (handled by build_simple)
        if let Some(mat) = find(r"^[\(\)<>\+\*/\-,]", text) {
            let r = build_simple(mat.as_str());
            let delta = mat.end();
            self.move_cursor(delta);
            return Ok(r);
        }
        return Err(LexerError::UnknownToken(
            if let Some(mat) = find(r"[^\s]+", text) {
                mat.as_str().to_owned()
            } else {
                text.chars().take(MAX_SNIPPET_LENGTH).collect()
            },
            self.loc,
        ));
    }

    pub fn get_all_tokens(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = Default::default();
        while self.has_next() {
            match self.next_token() {
                Ok(token) => tokens.push(token),
                Err(LexerError::Eof) => break,
                Err(error) => return Err(error),
            }
        }
        Ok(tokens)
    }

    pub fn peek_rest(&mut self) -> Result<Vec<Token>, LexerError> {
        let cursor = self.cursor;
        let tokens = self.get_all_tokens();
        self.cursor = cursor;
        tokens
    }

    pub fn peek(&mut self) -> Result<Token, LexerError> {
        let cursor = self.cursor;
        let tokens = self.next_token();
        self.cursor = cursor;
        tokens
    }
}

fn find<'a>(pattern: &str, source: &'a str) -> Option<regex::Match<'a>> {
    let re = Regex::new(pattern).expect("Invalid regular expression");
    re.find(source)
}

fn build_simple(string: &str) -> Token {
    match string {
        "(" => Token::LeftParen,
        ")" => Token::RightParen,
        "<" => Token::LeftAngle,
        ">" => Token::RightAngle,
        "+" => Token::Plus,
        "-" => Token::Minus,
        "*" => Token::Star,
        "/" => Token::Slash,
        "," => Token::Comma,
        _ => panic!("Unreachable"),
    }
}

/*



























*/
// enum TokenType {
//     OpenParen,
//     CloseParen,
//     Plus,
//     Minus,
//     Star,
//     Slash,
//     Bar,
//     Literal(Value),
//     Identifier(String),
// }

// struct Value {}
// struct Location {
//     line: u32,
//     col: u32,
// }

// struct Token {
//     location: Location,
//     token: TokenType,
// }

// fn tokenize(line: &str) -> VecDeque<Token> {
//     let mut buffer: String = String::new();
//     let mut chars: VecDeque<char> = line.chars().collect();
//     loop {
//         let Some(&c) = chars.front();
//         match c {
//             w if w.is_whitespace() => continue,
//             n if n.is_numeric() => {}
//             _ => {}
//         }
//     }
//     todo!()
// }
