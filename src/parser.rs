use std::rc::Rc;

use crate::ast;
use crate::error::{Error, LexerError, ParserError};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    token: Option<Token>,
    lexer: Lexer,
}
impl Parser {
    pub fn new(text: String) -> Self {
        Parser {
            token: None,
            lexer: Lexer::new(text),
        }
    }

    // error reporting when needed
    pub fn gen_ast(&mut self) -> Result<ast::Ast, Error> {
        let mut ast: ast::Ast = Default::default();
        while self.lexer.has_next() {
            ast.push(ast::AstNode::Expression(self.parse_expression()?));
        }
        Ok(ast)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        // println!("parsing statement");
        todo!()
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, Error> {
        // println!("parsing expression");
        let lhs: ast::Term = self.parse_term()?;
        Ok(match self.token()? {
            Token::Plus => {
                self.consume();
                ast::Expression::Add(lhs, Rc::new(self.parse_expression()?))
            }
            Token::Minus => {
                self.consume();
                ast::Expression::Subtract(lhs, Rc::new(self.parse_expression()?))
            }
            _ => ast::Expression::Term(lhs),
        })
    }

    fn parse_term(&mut self) -> Result<ast::Term, Error> {
        // println!("parsing term");
        let lhs: ast::Factor = self.parse_factor()?;
        Ok(match self.token()? {
            Token::Star => {
                self.consume();
                ast::Term::Multiply(lhs, Rc::new(self.parse_term()?))
            }
            Token::Slash => {
                self.consume();
                ast::Term::Divide(lhs, Rc::new(self.parse_term()?))
            }
            _ => ast::Term::Factor(lhs),
        })
    }

    fn parse_factor(&mut self) -> Result<ast::Factor, Error> {
        // println!("parsing factor");
        // println!("rest: {:?}", self.lexer.peek_rest());
        Ok(match self.token()? {
            // Token::Identifier(id) => {self.advance_token();ast::Factor(ast::Identifier(id))},
            Token::LeftParen => {
                self.consume();
                let parenthesized = self.parse_expression()?;
                if let Token::RightParen = self.token()? {
                    self.consume();
                    ast::Factor::Expression(Rc::new(parenthesized))
                } else {
                    Err(ParserError::Error("missing closing parenthesis".to_owned()))?
                }
            }
            Token::Minus => {
                self.consume();
                ast::Factor::Negate(Rc::new(self.parse_factor()?))
            }
            Token::Literal(literal) => {
                self.consume();
                ast::Factor::Literal(literal)
            }
            _ => Err(ParserError::Error(format!(
                "unable to parse factor {:?}, {:?}",
                self.token(),
                self.lexer.get_all_tokens()
            )))?,
        })
    }

    fn token(&mut self) -> Result<Token, Error> {
        if self.token.is_none() {
            self.advance_token()?;
        }
        let r = self.token.clone().unwrap();
        // println!("{:?}", r);
        Ok(r)
    }

    fn consume(&mut self) {
        if self.token.is_none() {
            panic!("tried to consume same token twice")
        }
        self.token = None
    }

    fn advance_token(&mut self) -> Result<(), Error> {
        // println!("{:?}", self.lexer.loc());
        self.token = match self.lexer.next_token() {
            Ok(v) => Some(v),
            Err(LexerError::Eof) => Some(Token::Eof),
            Err(e) => Err(e)?,
        };
        Ok(())
    }
}
