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
            ast.push(self.parse_ast_node()?);
        }
        Ok(ast)
    }

    fn parse_ast_node(&mut self) -> Result<ast::AstNode, Error> {
        Ok(ast::AstNode::Statement(self.parse_statement()?))
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
            return Err(ParserError::Error(
                format!(
                    "Expected semicolon at end of statement, found {:?} instead",
                    self.token()
                )
                .to_owned(),
            )
            .into());
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, Error> {
        // println!("parsing expression");
        let lhs: ast::Term = self.parse_term()?;
        Ok(match self.token()? {
            Token::Plus => {
                self.consume();
                ast::Expression::Add(lhs, Box::new(self.parse_expression()?))
            }
            Token::Minus => {
                self.consume();
                ast::Expression::Subtract(lhs, Box::new(self.parse_expression()?))
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
                ast::Term::Multiply(lhs, Box::new(self.parse_term()?))
            }
            Token::Slash => {
                self.consume();
                ast::Term::Divide(lhs, Box::new(self.parse_term()?))
            }
            _ => ast::Term::Factor(lhs),
        })
    }

    fn parse_factor(&mut self) -> Result<ast::Factor, Error> {
        // println!("parsing factor");
        // println!("rest: {:?}", self.lexer.peek_rest());
        Ok(match self.token()? {
            // Token::Identifier(id) => {self.advance_token();ast::Factor(ast::Identifier(id))},
            Token::Lambda => self.parse_lambda_expression()?,
            Token::LeftParen => {
                self.consume();
                let parenthesized = self.parse_expression()?;
                if let Token::RightParen = self.token()? {
                    self.consume();
                    ast::Factor::Expression(Box::new(parenthesized))
                } else {
                    Err(ParserError::Error("missing closing parenthesis".to_owned()))?
                }
            }
            Token::Minus => {
                self.consume();
                ast::Factor::Negate(Box::new(self.parse_factor()?))
            }
            Token::Literal(literal) => {
                self.consume();
                ast::Factor::Literal(literal)
            }
            Token::Identifier(name) => {
                if let Token::LeftParen = self.lexer.peek()? {
                    let x = self.parse_function_call()?;
                    // println!("{:?}", x);
                    x
                } else {
                    self.consume();
                    ast::Factor::Variable(ast::Identifier { name })
                }
            }
            _ => Err(ParserError::Error(format!(
                "unable to parse factor {:?}, {:?}",
                self.token(),
                self.lexer.get_all_tokens()
            )))?,
        })
    }

    fn parse_block(&mut self) -> Result<ast::Block, Error> {
        if !self.token()?.kind_eq(&Token::LeftBrace) {
            return Err(ParserError::Error(
                format!(
                    "Expected block beginning with `{{` found {:?}",
                    self.token()?
                )
                .to_owned(),
            )
            .into());
        }
        self.consume();
        let mut block: Vec<ast::AstNode> = Default::default();
        while !self.token()?.kind_eq(&Token::RightBrace) {
            block.push(self.parse_ast_node()?);
        }
        self.consume();

        Ok(ast::Block(block))
    }

    fn parse_lambda_expression(&mut self) -> Result<ast::Factor, Error> {
        if !self.token()?.kind_eq(&Token::Lambda) {
            return Err(ParserError::Error(
                "Attempted to parse lambda expression but failed to find keyworld `lambda`"
                    .to_owned(),
            )
            .into());
        }
        self.consume();
        let mut params: Vec<ast::Identifier> = Default::default();
        if !self.token()?.kind_eq(&Token::LeftParen) {
            return Err(ParserError::Error(
                "Expected parameter list after keyworld `lambda`".to_owned(),
            )
            .into());
        }
        self.consume();
        // TODO: this feels too complicated
        while !self.token()?.kind_eq(&Token::RightParen) {
            if let Token::RightParen = self.token()? {
                self.consume();
                break;
            }
            params.push(self.parse_identifier()?);
            if let Token::Comma = self.token()? {
                self.consume();
                continue;
            } else if let Token::RightParen = self.token()? {
                break;
            } else {
                return Err(
                    ParserError::Error("Expected comma or close parenthesis".to_owned()).into(),
                );
            }
        }
        self.consume();

        let block = self.parse_block()?;

        Ok(ast::Factor::Lambda(params, block))
    }

    fn parse_function_call(&mut self) -> Result<ast::Factor, Error> {
        if let Token::Identifier(name) = self.token()? {
            // println!("{:?}", self.token()?);
            self.consume();
            if let Token::LeftParen = self.token()? {
                // println!("{:?}", self.token()?);
                self.consume();
                let mut args: Vec<ast::Expression> = Default::default();
                // TODO: this feels too complicated
                while !self.token()?.kind_eq(&Token::RightParen) {
                    if let Token::RightParen = self.token()? {
                        self.consume();
                        break;
                    }
                    args.push(self.parse_expression()?);
                    if let Token::Comma = self.token()? {
                        self.consume();
                        continue;
                    } else {
                        if let Token::RightParen = self.token()? {
                            break;
                        }
                        Err(ParserError::Error(
                            "Expected comma or close parenthesis".to_owned(),
                        ))?;
                    }
                }
                self.consume();
                return Ok(ast::Factor::FunctionCall(ast::Identifier { name }, args));
            }
        }
        Err(ParserError::Error("Expected function call".to_owned()).into())
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
        // let loc = self.lexer.loc();
        self.token = match self.lexer.next_token() {
            Ok(v) => Some(v),
            Err(LexerError::Eof) => Some(Token::Eof),
            Err(e) => Err(e)?,
        };
        // println!("{:?} {:?}", self.token, loc);
        Ok(())
    }
}
