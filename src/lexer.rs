extern crate regex;

pub use crate::ast::Literal;
use crate::error::{LexerError, Loc};
use regex::Regex;

#[derive(Debug, Clone)]
pub enum Token {
    Literal(Literal),
    Identifier(String),
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
    Equal,
    Semicolon,
    Let,
    Set,
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
        let text: String = self.text.split_at(self.cursor).1.to_owned();
        if text.len() == 0 {
            return Err(LexerError::Eof);
        }
        let mut match_token = |pattern, fun: fn(&str) -> Token| {
            if let Some(mat) = find(pattern, &text) {
                self.loc.line += 1;
                let delta = mat.end();
                self.move_cursor(delta);
                Some(fun(mat.as_str()))
            } else {
                None
            }
        };
        // update loc on newline
        if text.starts_with('\n') {
            self.loc.line += 1;
            self.move_cursor(1);
            return self.next_token();
        }
        // skip whitespace
        if let Some(mat) = find(r"^\s+", &text) {
            let delta = mat.end();
            self.move_cursor(delta);
            // either this or return whitespace error?
            return self.next_token();
        }
        // grab integers
        let t: Option<Token> = match_token(r"^\d+", |mat| {
            let int: i32 = mat.parse().unwrap();
            Token::Literal(Literal::IntLiteral(int))
        }).
        // grab quotes
        // TODO: Handle escape characters
        or(match_token(r#"^"[^"]*""#, |mat| {
            Token::Literal(Literal::StringLiteral(
                mat.strip_prefix('"')
                    .unwrap()
                    .strip_suffix('"')
                    .unwrap()
                    .to_owned(),
            ))
        })).
        // Assignment `:=`, let, set
        or(match_token(r"^:=", |_| Token::ColonEqual)).
        or(match_token(r"^let", |_| Token::Let)).
        or(match_token(r"^set", |_| Token::Set)).
        // all single character tokens (handled by build_simple)
        or(match_token(r"^[\(\)<>\+\*/\-,;=]", |mat| build_simple(mat))).
        or(match_token(r"^[a-zA-Z_][a-zA-Z_\d]*", |mat| { Token::Identifier(mat.to_owned())}));

        return t.ok_or(LexerError::UnknownToken(
            if let Some(mat) = find(r"[^\s]+", &text) {
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
        ";" => Token::Semicolon,
        "=" => Token::Equal,
        _ => panic!("Unreachable"),
    }
}
