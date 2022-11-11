extern crate regex;

use std::fmt::Display;

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
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Plus,
    PlusPlus,
    Minus,
    Star,
    Slash,
    Comma,
    ColonEqual,
    Equal,
    Semicolon,
    Let,
    Set,
    Lambda,
    While,
    If,
    Else,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    SlashEqual,
    Tilde,
    Question,
    Eof,
}
impl Token {
    pub fn kind_eq(&self, other: &Token) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Literal(literal) => return write!(f, "{}", literal),
                Self::Identifier(string) => return write!(f, "Identifier `{}`", string),
                Self::LeftParen => "`(`",
                Self::RightParen => "`)`",
                Self::LeftAngle => "`<`",
                Self::RightAngle => "`>`",
                Self::LeftBracket => "`[`",
                Self::RightBracket => "`]`",
                Self::LeftBrace => "`{`",
                Self::RightBrace => "`}`",
                Self::Plus => "`+`",
                Self::PlusPlus => "`++`",
                Self::Minus => "`-`",
                Self::Star => "`*`",
                Self::Slash => "`/`",
                Self::Comma => "`,`",
                Self::ColonEqual => "`:=`",
                Self::Equal => "`=`",
                Self::Semicolon => "`;`",
                Self::Let => "`let`",
                Self::Set => "`set`",
                Self::Lambda => "`lambda`",
                Self::While => "`while`",
                Self::If => "`if`",
                Self::Else => "`else`",
                Self::EqualEqual => "`==`",
                Self::LessEqual => "`<=`",
                Self::GreaterEqual => "`>=`",
                Self::SlashEqual => "`/=`",
                Self::Tilde => "`~`",
                Self::Question => "`?`",
                Self::Eof => "[End Of File]",
            }
        )
    }
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
        self.cursor < self.text.trim_end().len()
    }

    fn move_cursor(&mut self, update: usize) {
        self.cursor += update;
        self.loc.col += update; // TODO: Doesn't handle newlines
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if !self.has_next() {
            if self.cursor == self.text.len() - 1 {
                self.cursor += 1;
            }
        }
        // text = string[cursor:]
        let text: String = self.text.split_at(self.cursor).1.to_owned();
        if text.len() == 0 {
            return Ok(Token::Eof);
        }
        let mut match_token_pattern = |pattern, fun: fn(&str) -> Token| {
            if let Some(mat) = find(pattern, &text) {
                // self.loc.line += 1; // TODO: this is not right lol
                let delta = mat.end();
                self.move_cursor(delta);
                Some(fun(mat.as_str()))
            } else {
                None
            }
        };
        fn match_token_simple(
            pattern: &str,
            token: Token,
            text: &str,
            lex: &mut Lexer,
        ) -> Option<Token> {
            if let Some(mat) = find(pattern, text) {
                // self.loc.line += 1; // TODO: this is not right lol
                let delta = mat.end();
                lex.move_cursor(delta);
                Some(token)
            } else {
                None
            }
        }
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
            return self.next_token();
        }
        // grab integers
        let t: Option<Token> = match_token_pattern(r"^\d+", |mat| {
            let int: i32 = mat.parse().unwrap();
            Token::Literal(Literal::IntLiteral(int))
        }).
        // grab quotes
        // TODO: Handle escape characters
        or_else(|| match_token_pattern(r#"^"[^"]*""#, |mat| {
            Token::Literal(Literal::StringLiteral(
                mat.strip_prefix('"')
                    .unwrap()
                    .strip_suffix('"')
                    .unwrap()
                    .to_owned(),
            ))
        })).
        // keywords
        or_else(|| match_token_pattern(r"^let\b", |_| Token::Let)).
        or_else(|| match_token_pattern(r"^set\b", |_| Token::Set)).
        or_else(|| match_token_pattern(r"^while\b", |_| Token::While)).
        or_else(|| match_token_pattern(r"^if\b", |_| Token::If)).
        or_else(|| match_token_pattern(r"^else\b", |_| Token::Else)).
        or_else(|| match_token_pattern(r"^lambda\b", |_| Token::Lambda)).
        or_else(|| match_token_pattern(r"^true\b", |_| Token::Literal(Literal::BooleanLiteral(true)))).
        or_else(|| match_token_pattern(r"^false\b", |_| Token::Literal(Literal::BooleanLiteral(false)))).
        // identifiers
        or_else(|| match_token_pattern(r"^[a-zA-Z_][a-zA-Z_\d]*", |mat| { Token::Identifier(mat.to_owned()) })).
        // anything else that couldn't be confused for an identifier
        or_else(|| {
            for (pattern, token) in [
                (r"^==", Token::EqualEqual),
                (r"^/=", Token::SlashEqual),
                (r"^>=", Token::GreaterEqual),
                (r"^<=", Token::LessEqual),
                (r"^:=", Token::ColonEqual),
                (r"^\+\+", Token::PlusPlus),
                (r"^\(", Token::LeftParen),
                (r"^\)", Token::RightParen),
                (r"^<", Token::LeftAngle),
                (r"^>", Token::RightAngle),
                (r"^\[", Token::LeftBracket),
                (r"^\]", Token::RightBracket),
                (r"^\{", Token::LeftBrace),
                (r"^\}", Token::RightBrace),
                (r"^\+", Token::Plus),
                (r"^-", Token::Minus),
                (r"^\*", Token::Star),
                (r"^/", Token::Slash),
                // (r"^\\", Token::Backslash),
                (r"^~", Token::Tilde),
                (r"^\?", Token::Question),
                (r"^,", Token::Comma),
                (r"^;", Token::Semicolon),
                (r"^=", Token::Equal),
            ] {
                let t = match_token_simple(pattern, token, &text, self);
                if t.is_some() {
                    return t;
                }
            }
            None
        });

        return t.ok_or_else(|| {
            LexerError::UnknownToken(
                if let Some(mat) = find(r"[^\s]+", &text) {
                    mat.as_str().to_owned()
                } else {
                    text.chars().take(MAX_SNIPPET_LENGTH).collect()
                },
                self.loc,
            )
        });
    }

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
