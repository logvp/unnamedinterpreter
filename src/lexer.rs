use std::{collections::VecDeque, fmt::Display};

pub use crate::ast::Literal;
use crate::error::{LexerError, Loc};

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
    Dot,
    Comma,
    ColonEqual,
    Equal,
    Semicolon,
    Var,
    Let,
    Set,
    With,
    New,
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
    Newline,
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
                Self::Dot => "`.`",
                Self::Comma => "`,`",
                Self::ColonEqual => "`:=`",
                Self::Equal => "`=`",
                Self::Semicolon => "`;`",
                Self::Let => "`let`",
                Self::Var => "`var`",
                Self::Set => "`set`",
                Self::With => "`with`",
                Self::New => "`new`",
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
                Self::Newline => panic!("[Newline] should be transparent"),
            }
        )
    }
}

#[derive(Debug)]
pub struct Lexer {
    tokens: VecDeque<Token>,
}
impl Lexer {
    pub fn lex(text: String) -> Result<Self, LexerError> {
        let mut chars = text.chars().peekable();
        let mut tokens = VecDeque::new();
        let mut buffer = String::new();
        let mut loc = Loc::default();
        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    loc.line += 1;
                    loc.col = 0;
                    // consecutive newlines are not needed
                    if !matches!(tokens.back(), Some(Token::Newline)) {
                        tokens.push_back(Token::Newline)
                    }
                }
                _ if c.is_whitespace() => {
                    loc.col += 1;
                }
                '0' if chars.next_if_eq(&'x').is_some() => {
                    buffer.clear();
                    while let Some(digit) = chars.next_if(|d| d.is_alphanumeric()) {
                        buffer.push(digit);
                    }
                    match i32::from_str_radix(&buffer, 0x10) {
                        Ok(int) => tokens.push_back(Token::Literal(Literal::Integer(int))),
                        Err(_) => return Err(LexerError::BadHexLiteral(buffer, loc)),
                    }
                }
                '0' if chars.next_if_eq(&'b').is_some() => {
                    buffer.clear();
                    while let Some(digit) = chars.next_if(|d| d.is_alphanumeric()) {
                        buffer.push(digit);
                    }
                    match i32::from_str_radix(&buffer, 0b10) {
                        Ok(int) => tokens.push_back(Token::Literal(Literal::Integer(int))),
                        Err(_) => return Err(LexerError::BadBinLiteral(buffer, loc)),
                    }
                }
                _ if c.is_digit(10) => {
                    buffer.clear();
                    buffer.push(c);
                    while let Some(digit) = chars.next_if(|d| d.is_digit(10)) {
                        buffer.push(digit);
                    }
                    tokens.push_back(Token::Literal(Literal::Integer(buffer.parse().unwrap())))
                }
                '"' => {
                    buffer.clear();
                    while let Some(c) = chars.next_if(|&c| c != '"') {
                        // escape sequences
                        buffer.push(c);
                    }
                    if let Some('"') = chars.next() {
                        tokens.push_back(Token::Literal(Literal::String(buffer.clone())))
                    } else {
                        return Err(LexerError::UnterminatedStringLiteral(buffer, loc));
                    }
                }
                '<' if chars.next_if_eq(&'=').is_some() => tokens.push_back(Token::LessEqual),
                '>' if chars.next_if_eq(&'=').is_some() => tokens.push_back(Token::GreaterEqual),
                '+' if chars.next_if_eq(&'+').is_some() => tokens.push_back(Token::PlusPlus),
                ':' if chars.next_if_eq(&'=').is_some() => tokens.push_back(Token::ColonEqual),
                '=' if chars.next_if_eq(&'=').is_some() => tokens.push_back(Token::EqualEqual),
                '/' if chars.next_if_eq(&'=').is_some() => tokens.push_back(Token::SlashEqual),

                '<' => tokens.push_back(Token::LeftAngle),
                '>' => tokens.push_back(Token::RightAngle),
                '(' => tokens.push_back(Token::LeftParen),
                ')' => tokens.push_back(Token::RightParen),
                '[' => tokens.push_back(Token::LeftBracket),
                ']' => tokens.push_back(Token::RightBracket),
                '{' => tokens.push_back(Token::LeftBrace),
                '}' => tokens.push_back(Token::RightBrace),
                '=' => tokens.push_back(Token::Equal),
                '+' => tokens.push_back(Token::Plus),
                '-' => tokens.push_back(Token::Minus),
                '.' => tokens.push_back(Token::Dot),
                ',' => tokens.push_back(Token::Comma),
                '*' => tokens.push_back(Token::Star),
                '/' => tokens.push_back(Token::Slash),
                ';' => tokens.push_back(Token::Semicolon),
                '~' => tokens.push_back(Token::Tilde),
                '?' => tokens.push_back(Token::Question),

                '_' | 'a'..='z' | 'A'..='Z' => {
                    buffer.clear();
                    buffer.push(c);
                    while let Some(c) =
                        chars.next_if(|c| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                    {
                        buffer.push(c);
                    }
                    let tok = match buffer.as_str() {
                        "true" => Token::Literal(Literal::Boolean(true)),
                        "false" => Token::Literal(Literal::Boolean(false)),
                        "var" => Token::Var,
                        "let" => Token::Let,
                        "set" => Token::Set,
                        "with" => Token::With,
                        "new" => Token::New,
                        "lambda" => Token::Lambda,
                        "while" => Token::While,
                        "if" => Token::If,
                        "else" => Token::Else,
                        _ => Token::Identifier(buffer.clone()),
                    };
                    tokens.push_back(tok)
                }
                _ => {
                    buffer.clear();
                    buffer.push(c);
                    while let Some(c) = chars.next_if(|c| !c.is_whitespace()) {
                        buffer.push(c);
                    }
                    return Err(LexerError::UnknownToken(buffer, loc));
                }
            }
        }
        Ok(Lexer { tokens })
    }

    pub fn next_token(&mut self) -> Token {
        match self.tokens.pop_front() {
            Some(Token::Newline) => self.next_token(),
            Some(tok) => tok,
            None => Token::Eof,
        }
    }

    pub fn peek(&self) -> &Token {
        match self
            .tokens
            .iter()
            .filter(|x| !matches!(x, Token::Newline))
            .next()
        {
            Some(Token::Newline) => self.peek_over(),
            Some(tok) => tok,
            None => &Token::Eof,
        }
    }

    pub fn peek_over(&self) -> &Token {
        match self
            .tokens
            .iter()
            .filter(|x| !matches!(x, Token::Newline))
            .nth(1)
        {
            Some(tok) => tok,
            None => &Token::Eof,
        }
    }

    // next and convert Newline To Semicolon
    pub fn next_token_nts(&mut self) -> Token {
        match self.tokens.pop_front() {
            Some(Token::Newline) => Token::Semicolon,
            Some(tok) => tok,
            None => Token::Eof,
        }
    }

    // peek anc convert Newline To Semicolon
    pub fn peek_nts(&self) -> &Token {
        match self.tokens.front() {
            Some(Token::Newline) => &Token::Semicolon,
            Some(tok) => tok,
            None => &Token::Eof,
        }
    }
}
