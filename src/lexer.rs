use std::{collections::VecDeque, fmt::Display, rc::Rc};

pub use crate::ast::Literal;
use crate::error::{LexerError, Loc};

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum TokenKind {
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
impl TokenKind {
    pub fn kind_eq(&self, other: &TokenKind) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }
}
impl Display for TokenKind {
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
    end: Loc,
}
impl Lexer {
    pub fn lex(text: &str, filename: Option<Rc<str>>) -> Result<Self, LexerError> {
        let mut chars = text.chars().peekable();
        let mut tokens: VecDeque<Token> = VecDeque::new();
        let mut buffer = String::new();
        let mut loc = Loc::new(filename);
        fn add_tok(tokens: &mut VecDeque<Token>, kind: TokenKind, loc: &Loc) {
            tokens.push_back(Token {
                kind,
                loc: loc.clone(),
            })
        }
        while let Some(c) = chars.next() {
            match c {
                '\n' => {
                    loc.line += 1;
                    loc.col = 0;
                    // consecutive newlines are not needed
                    if !matches!(
                        tokens.back(),
                        Some(Token {
                            kind: TokenKind::Newline,
                            ..
                        })
                    ) {
                        add_tok(&mut tokens, TokenKind::Newline, &loc)
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
                        Ok(int) => {
                            add_tok(&mut tokens, TokenKind::Literal(Literal::Integer(int)), &loc)
                        }
                        Err(_) => return Err(LexerError::BadHexLiteral(buffer, loc)),
                    }
                }
                '0' if chars.next_if_eq(&'b').is_some() => {
                    buffer.clear();
                    while let Some(digit) = chars.next_if(|d| d.is_alphanumeric()) {
                        buffer.push(digit);
                    }
                    match i32::from_str_radix(&buffer, 0b10) {
                        Ok(int) => {
                            add_tok(&mut tokens, TokenKind::Literal(Literal::Integer(int)), &loc)
                        }
                        Err(_) => return Err(LexerError::BadBinLiteral(buffer, loc)),
                    }
                }
                _ if c.is_digit(10) => {
                    buffer.clear();
                    buffer.push(c);
                    while let Some(digit) = chars.next_if(|d| d.is_digit(10)) {
                        buffer.push(digit);
                    }
                    add_tok(
                        &mut tokens,
                        TokenKind::Literal(Literal::Integer(buffer.parse().unwrap())),
                        &loc,
                    )
                }
                '"' => {
                    buffer.clear();
                    while let Some(c) = chars.next_if(|&c| c != '"') {
                        // escape sequences
                        buffer.push(c);
                    }
                    if let Some('"') = chars.next() {
                        add_tok(
                            &mut tokens,
                            TokenKind::Literal(Literal::String(buffer.clone())),
                            &loc,
                        )
                    } else {
                        return Err(LexerError::UnterminatedStringLiteral(buffer, loc));
                    }
                }
                '<' if chars.next_if_eq(&'=').is_some() => {
                    add_tok(&mut tokens, TokenKind::LessEqual, &loc)
                }
                '>' if chars.next_if_eq(&'=').is_some() => {
                    add_tok(&mut tokens, TokenKind::GreaterEqual, &loc)
                }
                '+' if chars.next_if_eq(&'+').is_some() => {
                    add_tok(&mut tokens, TokenKind::PlusPlus, &loc)
                }
                ':' if chars.next_if_eq(&'=').is_some() => {
                    add_tok(&mut tokens, TokenKind::ColonEqual, &loc)
                }
                '=' if chars.next_if_eq(&'=').is_some() => {
                    add_tok(&mut tokens, TokenKind::EqualEqual, &loc)
                }
                '/' if chars.next_if_eq(&'=').is_some() => {
                    add_tok(&mut tokens, TokenKind::SlashEqual, &loc)
                }

                '<' => add_tok(&mut tokens, TokenKind::LeftAngle, &loc),
                '>' => add_tok(&mut tokens, TokenKind::RightAngle, &loc),
                '(' => add_tok(&mut tokens, TokenKind::LeftParen, &loc),
                ')' => add_tok(&mut tokens, TokenKind::RightParen, &loc),
                '[' => add_tok(&mut tokens, TokenKind::LeftBracket, &loc),
                ']' => add_tok(&mut tokens, TokenKind::RightBracket, &loc),
                '{' => add_tok(&mut tokens, TokenKind::LeftBrace, &loc),
                '}' => add_tok(&mut tokens, TokenKind::RightBrace, &loc),
                '=' => add_tok(&mut tokens, TokenKind::Equal, &loc),
                '+' => add_tok(&mut tokens, TokenKind::Plus, &loc),
                '-' => add_tok(&mut tokens, TokenKind::Minus, &loc),
                '.' => add_tok(&mut tokens, TokenKind::Dot, &loc),
                ',' => add_tok(&mut tokens, TokenKind::Comma, &loc),
                '*' => add_tok(&mut tokens, TokenKind::Star, &loc),
                '/' => add_tok(&mut tokens, TokenKind::Slash, &loc),
                ';' => add_tok(&mut tokens, TokenKind::Semicolon, &loc),
                '~' => add_tok(&mut tokens, TokenKind::Tilde, &loc),
                '?' => add_tok(&mut tokens, TokenKind::Question, &loc),

                '_' | 'a'..='z' | 'A'..='Z' => {
                    buffer.clear();
                    buffer.push(c);
                    while let Some(c) =
                        chars.next_if(|c| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                    {
                        buffer.push(c);
                    }
                    let tok = match buffer.as_str() {
                        "true" => TokenKind::Literal(Literal::Boolean(true)),
                        "false" => TokenKind::Literal(Literal::Boolean(false)),
                        "var" => TokenKind::Var,
                        "let" => TokenKind::Let,
                        "set" => TokenKind::Set,
                        "with" => TokenKind::With,
                        "new" => TokenKind::New,
                        "lambda" => TokenKind::Lambda,
                        "while" => TokenKind::While,
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        _ => TokenKind::Identifier(buffer.clone()),
                    };
                    add_tok(&mut tokens, tok, &loc)
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
        Ok(Lexer { tokens, end: loc })
    }

    pub fn next_token(&mut self) -> Token {
        match self.tokens.pop_front() {
            Some(Token {
                kind: TokenKind::Newline,
                ..
            }) => self.next_token(),
            Some(tok) => tok,
            None => Token {
                kind: TokenKind::Eof,
                loc: self.end.clone(),
            },
        }
    }

    pub fn peek(&self) -> &TokenKind {
        match self
            .tokens
            .iter()
            .filter(|x| {
                !matches!(
                    x,
                    Token {
                        kind: TokenKind::Newline,
                        ..
                    }
                )
            })
            .next()
        {
            Some(Token { kind, .. }) => kind,
            None => &TokenKind::Eof,
        }
    }

    pub fn peek_over(&self) -> &TokenKind {
        match self
            .tokens
            .iter()
            .filter(|x| {
                !matches!(
                    x,
                    Token {
                        kind: TokenKind::Newline,
                        ..
                    }
                )
            })
            .nth(1)
        {
            Some(Token { kind, .. }) => kind,
            None => &TokenKind::Eof,
        }
    }

    // next and convert Newline To Semicolon
    pub fn next_token_nts(&mut self) -> Token {
        match self.tokens.pop_front() {
            Some(Token {
                kind: TokenKind::Newline,
                loc,
            }) => Token {
                kind: TokenKind::Semicolon,
                loc,
            },
            Some(tok) => tok,
            None => Token {
                kind: TokenKind::Eof,
                loc: self.end.clone(),
            },
        }
    }

    // peek anc convert Newline To Semicolon
    pub fn peek_nts(&self) -> &TokenKind {
        match self.tokens.front() {
            Some(Token {
                kind: TokenKind::Newline,
                ..
            }) => &TokenKind::Semicolon,
            Some(Token { kind, .. }) => kind,
            None => &TokenKind::Eof,
        }
    }

    pub fn peek_all_tokens(&self) -> impl Iterator<Item = &Token> {
        self.tokens.iter()
    }
}
