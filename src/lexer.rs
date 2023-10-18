use std::{collections::VecDeque, fmt::Display, iter::Peekable, rc::Rc};

pub use crate::ast::Literal;
use crate::error::{LexicalError, Loc};

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
}

#[derive(Debug)]
pub enum TokenKind {
    Literal(Literal),
    Identifier(Rc<str>),
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
    pub fn lex(text: &str, filename: Option<Rc<str>>) -> Result<Self, LexicalError> {
        let mut chars = text.chars().peekable();
        let mut tokens: VecDeque<Token> = VecDeque::new();
        let mut buffer = String::new();
        let mut loc = Loc::new(filename);
        fn build_buffer_while(
            buffer: &mut String,
            loc: &mut Loc,
            initial: Option<char>,
            chars: &mut Peekable<impl Iterator<Item = char>>,
            func: fn(&char) -> bool,
        ) {
            buffer.clear();
            if let Some(c) = initial {
                buffer.push(c);
            }
            while let Some(c) = chars.next_if(func) {
                loc.inc(c);
                buffer.push(c);
            }
        }
        fn next_if_eq(
            chars: &mut Peekable<impl Iterator<Item = char>>,
            ch: char,
            loc: &mut Loc,
        ) -> bool {
            if let Some(c) = chars.next_if_eq(&ch) {
                loc.inc(c);
                true
            } else {
                false
            }
        }
        fn add_tok(tokens: &mut VecDeque<Token>, kind: TokenKind, loc: &Loc) {
            tokens.push_back(Token {
                kind,
                loc: loc.clone(),
            })
        }
        while let Some(c) = chars.next() {
            loc.inc(c);
            let token = match c {
                '\n' => {
                    // consecutive newlines are not needed
                    if let Some(Token {
                        kind: TokenKind::Newline,
                        loc: _,
                    }) = tokens.back()
                    {
                        continue;
                    } else {
                        TokenKind::Newline
                    }
                }
                _ if c.is_whitespace() => continue,
                '0' if next_if_eq(&mut chars, 'x', &mut loc) => {
                    build_buffer_while(&mut buffer, &mut loc, None, &mut chars, |d| {
                        d.is_alphanumeric()
                    });
                    match i32::from_str_radix(&buffer, 0x10) {
                        Ok(int) => TokenKind::Literal(Literal::Integer(int)),
                        Err(_) => return Err(LexicalError::BadHexLiteral(buffer, loc)),
                    }
                }
                '0' if next_if_eq(&mut chars, 'b', &mut loc) => {
                    build_buffer_while(&mut buffer, &mut loc, None, &mut chars, |d| {
                        d.is_alphanumeric()
                    });
                    match i32::from_str_radix(&buffer, 0b10) {
                        Ok(int) => TokenKind::Literal(Literal::Integer(int)),
                        Err(_) => return Err(LexicalError::BadBinLiteral(buffer, loc)),
                    }
                }
                '0'..='9' => {
                    build_buffer_while(&mut buffer, &mut loc, Some(c), &mut chars, |d| {
                        d.is_ascii_digit()
                    });

                    TokenKind::Literal(Literal::Integer(buffer.parse().unwrap()))
                }
                '-' if next_if_eq(&mut chars, '-', &mut loc) => {
                    // chomp until the end of the line
                    while let Some(_) = chars.next_if(|&x| x != '\n') {}
                    continue;
                }
                '{' if next_if_eq(&mut chars, '-', &mut loc) => {
                    fn chomp_comment(
                        mut chars: &mut Peekable<impl Iterator<Item = char>>,
                        start_loc: Loc,
                    ) -> Result<Loc, LexicalError> {
                        let mut loc = start_loc.clone();
                        while let Some(c) = chars.next() {
                            match c {
                                '{' if next_if_eq(&mut chars, '-', &mut loc) => {
                                    loc = chomp_comment(chars, loc)?;
                                }
                                '-' if next_if_eq(&mut chars, '}', &mut loc) => return Ok(loc),
                                _ => loc.inc(c),
                            }
                        }
                        return Err(LexicalError::UnmatchedMultilineComment(start_loc));
                    }
                    loc = chomp_comment(&mut chars, loc)?;
                    continue;
                }
                '"' => {
                    buffer.clear();
                    while let Some(c) = chars.next_if(|&c| c != '"') {
                        loc.inc(c);
                        match c {
                            '\\' => {
                                if let Some(c) = chars.next() {
                                    loc.inc(c);
                                    match c {
                                        'n' => buffer.push('\n'),
                                        'r' => buffer.push('\r'),
                                        't' => buffer.push('\t'),
                                        '"' => buffer.push('"'),
                                        '\\' => buffer.push('\\'),
                                        _ => return Err(LexicalError::InvalidEscape(c, loc)),
                                    }
                                }
                            }
                            _ => buffer.push(c),
                        }
                    }
                    if let Some(c @ '"') = chars.next() {
                        loc.inc(c);

                        TokenKind::Literal(Literal::String(Rc::from(buffer.clone())))
                    } else {
                        return Err(LexicalError::UnterminatedStringLiteral(buffer, loc));
                    }
                }
                '<' if next_if_eq(&mut chars, '=', &mut loc) => TokenKind::LessEqual,
                '>' if next_if_eq(&mut chars, '=', &mut loc) => TokenKind::GreaterEqual,
                '+' if next_if_eq(&mut chars, '+', &mut loc) => TokenKind::PlusPlus,
                ':' if next_if_eq(&mut chars, '=', &mut loc) => TokenKind::ColonEqual,
                '=' if next_if_eq(&mut chars, '=', &mut loc) => TokenKind::EqualEqual,
                '/' if next_if_eq(&mut chars, '=', &mut loc) => TokenKind::SlashEqual,

                '<' => TokenKind::LeftAngle,
                '>' => TokenKind::RightAngle,
                '(' => TokenKind::LeftParen,
                ')' => TokenKind::RightParen,
                '[' => TokenKind::LeftBracket,
                ']' => TokenKind::RightBracket,
                '{' => TokenKind::LeftBrace,
                '}' => TokenKind::RightBrace,
                '=' => TokenKind::Equal,
                '+' => TokenKind::Plus,
                '-' => TokenKind::Minus,
                '.' => TokenKind::Dot,
                ',' => TokenKind::Comma,
                '*' => TokenKind::Star,
                '/' => TokenKind::Slash,
                ';' => TokenKind::Semicolon,
                '~' => TokenKind::Tilde,
                '?' => TokenKind::Question,

                '_' | 'a'..='z' | 'A'..='Z' => {
                    build_buffer_while(
                        &mut buffer,
                        &mut loc,
                        Some(c),
                        &mut chars,
                        |c| matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'),
                    );
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
                        _ => TokenKind::Identifier(Rc::from(buffer.clone())),
                    };
                    tok
                }
                _ => {
                    build_buffer_while(&mut buffer, &mut loc, Some(c), &mut chars, |c| {
                        !c.is_whitespace()
                    });
                    return Err(LexicalError::UnknownToken(buffer, loc));
                }
            };
            add_tok(&mut tokens, token, &loc)
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
        match self.tokens.iter().find(|x| {
            !matches!(
                x,
                Token {
                    kind: TokenKind::Newline,
                    ..
                }
            )
        }) {
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

    pub fn next_token_allow_newline(&mut self) -> Token {
        match self.tokens.pop_front() {
            Some(tok) => tok,
            None => Token {
                kind: TokenKind::Eof,
                loc: self.end.clone(),
            },
        }
    }
}
