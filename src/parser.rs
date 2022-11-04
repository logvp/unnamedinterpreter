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
            // println!("{:?}", self.lexer.peek_rest());
            ast.push(ast::AstNode::Statement(self.parse_statement()?));
        }
        Ok(ast)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, Error> {
        // println!("parsing statement");
        let body = match self.token()? {
            Token::Let => {
                self.consume();
                let identifier = self.parse_identifier()?;
                if let Token::Equal = self.token()? {
                    self.consume();
                    ast::Statement::Declaration(identifier, self.parse_expression()?)
                } else {
                    return Err(ParserError::Error(
                        "Expected equal sign after let identifier construct".to_owned(),
                    )
                    .into());
                }
            }
            Token::Set => {
                self.consume();
                let lvalue = self.parse_lvalue()?;
                if let Token::Equal = self.token()? {
                    self.consume();
                    ast::Statement::Assignment(lvalue, self.parse_expression()?)
                } else {
                    return Err(ParserError::Error(
                        "Expected equal sign after set identifier construct".to_owned(),
                    )
                    .into());
                }
            }
            _ => {
                if let Token::Identifier(_) = self.token()? {
                    if let Ok(Token::Equal) = self.lexer.peek() {
                        let lvalue = self.parse_lvalue()?;
                        // consume `=`
                        self.token()?;
                        self.consume();
                        return Ok(ast::Statement::Assignment(lvalue, self.parse_expression()?));
                    }
                    if let Ok(Token::ColonEqual) = self.lexer.peek() {
                        let lvalue = self.parse_identifier()?;
                        // consume `:=`
                        self.token()?;
                        self.consume();
                        return Ok(ast::Statement::Declaration(
                            lvalue,
                            self.parse_expression()?,
                        ));
                    }
                }
                ast::Statement::Expression(self.parse_expression()?)
            }
        };
        if let Token::Semicolon = self.token()? {
            self.consume();
            return Ok(body);
        } else {
            return Err(
                ParserError::Error("Expected semicolon at end of statement".to_owned()).into(),
            );
        }
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
            Token::Identifier(name) => {
                self.consume();
                ast::Factor::Variable(ast::Identifier { name })
            }
            _ => Err(ParserError::Error(format!(
                "unable to parse factor {:?}, {:?}",
                self.token(),
                self.lexer.get_all_tokens()
            )))?,
        })
    }

    fn parse_lvalue(&mut self) -> Result<ast::Lvalue, Error> {
        Ok(ast::Lvalue::Identifier(self.parse_identifier()?))
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier, Error> {
        if let Token::Identifier(name) = self.token()? {
            self.consume();
            Ok(ast::Identifier { name })
        } else {
            Err(ParserError::Error("Expected identifier name".to_owned()).into())
        }
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
        let loc = self.lexer.loc();
        self.token = match self.lexer.next_token() {
            Ok(v) => Some(v),
            Err(LexerError::Eof) => Some(Token::Eof),
            Err(e) => Err(e)?,
        };
        // println!("{:?} {:?}", self.token, loc);
        Ok(())
    }
}
