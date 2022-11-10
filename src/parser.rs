use crate::ast;
use crate::error::{Error, LexerError, SyntaxError};
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
                    return Err(SyntaxError::ExpectedTokenIn(
                        Token::Equal,
                        self.token()?,
                        ast::ConstructKind::Let,
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
                    return Err(SyntaxError::ExpectedTokenIn(
                        Token::Equal,
                        self.token()?,
                        ast::ConstructKind::Set,
                    )
                    .into());
                }
            }
            _ => {
                // Simple Assignment ( = )
                if let Ok(Token::Equal) = self.peek() {
                    let lvalue = self.parse_lvalue()?;
                    self.token()?;
                    self.consume();
                    ast::Statement::Assignment(lvalue, self.parse_expression()?)
                }
                // Simple Declaration ( := )
                else if let Ok(Token::ColonEqual) = self.peek() {
                    let lvalue = self.parse_identifier()?;
                    self.token()?;
                    self.consume();
                    ast::Statement::Declaration(lvalue, self.parse_expression()?)
                } else {
                    ast::Statement::Expression(self.parse_expression()?)
                }
            }
        };
        if let Token::Semicolon = self.token()? {
            self.consume();
            return Ok(body);
        } else {
            return Err(SyntaxError::ExpectedTokenIn(
                Token::Semicolon,
                self.token()?,
                ast::ConstructKind::Statement,
            )
            .into());
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, Error> {
        // println!("parsing expression");
        Ok(match self.token()? {
            Token::If => {
                self.consume();
                let cond = self.parse_parenthetical_expression()?;
                let body = self.parse_block()?;
                // TODO: else if
                let else_ = if let Token::Else = self.token()? {
                    self.consume();
                    self.parse_block()?
                } else {
                    Default::default()
                };
                ast::Expression::IfElse(Box::new(cond), body, else_)
            }
            Token::While => {
                self.consume();
                let cond = self.parse_parenthetical_expression()?;
                let body = self.parse_block()?;
                ast::Expression::While(Box::new(cond), body)
            }
            _ => {
                let lhs: ast::Term = self.parse_term()?;
                match self.token()? {
                    Token::Plus => {
                        self.consume();
                        ast::Expression::Add(lhs, Box::new(self.parse_expression()?))
                    }
                    Token::Minus => {
                        self.consume();
                        ast::Expression::Subtract(lhs, Box::new(self.parse_expression()?))
                    }
                    Token::EqualEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::Equal,
                        )
                    }
                    Token::LessEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::LessEqual,
                        )
                    }
                    Token::LeftAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::LessThan,
                        )
                    }
                    Token::GreaterEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::GreaterEqual,
                        )
                    }
                    Token::RightAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::GreaterThan,
                        )
                    }
                    Token::SlashEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression()?),
                            ast::Comparison::NotEqual,
                        )
                    }
                    _ => ast::Expression::Term(lhs),
                }
            }
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
            Token::PlusPlus => {
                self.consume();
                ast::Term::Concatenate(lhs, Box::new(self.parse_term()?))
            }
            _ => ast::Term::Factor(lhs),
        })
    }

    fn parse_factor(&mut self) -> Result<ast::Factor, Error> {
        // println!("parsing factor");
        // println!("rest: {:?}", self.lexer.peek_rest());
        let factor = match self.token()? {
            // Token::Identifier(id) => {self.advance_token();ast::Factor(ast::Identifier(id))},
            Token::Lambda => self.parse_lambda_expression()?,
            Token::LeftParen => {
                ast::Factor::Expression(Box::new(self.parse_parenthetical_expression()?))
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
                self.consume();
                ast::Factor::Variable(ast::Identifier { name })
            }
            _ => Err(SyntaxError::UnexpectedToken(self.token()?))?,
        };
        if let Token::LeftParen = self.token()? {
            self.parse_function_call(factor)
        } else {
            Ok(factor)
        }

        // todo!()
    }

    fn parse_parenthetical_expression(&mut self) -> Result<ast::Expression, Error> {
        if let Token::LeftParen = self.token()? {
            self.consume();
            let parenthesized = self.parse_expression()?;
            if let Token::RightParen = self.token()? {
                self.consume();
                Ok(parenthesized)
            } else {
                Err(SyntaxError::ExpectedToken(Token::RightParen, self.token()?))?
            }
        } else {
            Err(SyntaxError::ExpectedToken(Token::LeftParen, self.token()?))?
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block, Error> {
        if !self.token()?.kind_eq(&Token::LeftBrace) {
            return Err(SyntaxError::ExpectedTokenIn(
                Token::LeftBrace,
                self.token()?,
                ast::ConstructKind::Block,
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
            return Err(SyntaxError::ExpectedToken(Token::Lambda, self.token()?).into());
        }
        // println!("got lambda keyword");
        self.consume();
        let mut params: Vec<ast::Identifier> = Default::default();
        if !self.token()?.kind_eq(&Token::LeftParen) {
            return Err(SyntaxError::ExpectedConstructIn(
                ast::ConstructKind::ParameterList,
                ast::ConstructKind::Lambda,
            )
            .into());
        }
        self.consume();
        // TODO: this feels too complicated
        while !self.token()?.kind_eq(&Token::RightParen) {
            params.push(self.parse_identifier()?);
            if let Token::Comma = self.token()? {
                self.consume();
                continue;
            } else if let Token::RightParen = self.token()? {
                break;
            } else {
                Err(SyntaxError::ExpectedTheseButFound(
                    vec![Token::Comma, Token::RightParen],
                    self.token()?,
                ))?;
            }
        }
        self.consume(); // consume `)`

        let block = self.parse_block()?;

        Ok(ast::Factor::Lambda(params, block))
    }

    fn parse_function_call(&mut self, func: ast::Factor) -> Result<ast::Factor, Error> {
        let call = ast::Factor::FunctionCall(Box::new(func), self.parse_function_args()?);
        if let Token::LeftParen = self.token()? {
            self.parse_function_call(call)
        } else {
            Ok(call)
        }
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, Error> {
        if let Token::LeftParen = self.token()? {
            // // println!("{:?}", self.token()?);
            self.consume();
            let mut args: Vec<ast::Expression> = Default::default();
            while !self.token()?.kind_eq(&Token::RightParen) {
                args.push(self.parse_expression()?);
                // this allows for trailing commas. should that be an error?
                if let Token::Comma = self.token()? {
                    self.consume();
                    continue;
                } else if let Token::RightParen = self.token()? {
                    break;
                } else {
                    Err(SyntaxError::ExpectedTheseButFound(
                        vec![Token::Comma, Token::RightParen],
                        self.token()?,
                    ))?;
                }
            }
            self.consume(); // consume `)`
            Ok(args)
        } else {
            Err(SyntaxError::ExpectedTokenIn(
                Token::LeftParen,
                self.token()?,
                ast::ConstructKind::FunctionCall,
            )
            .into())
        }
    }

    fn parse_lvalue(&mut self) -> Result<ast::Lvalue, Error> {
        Ok(ast::Lvalue::Identifier(self.parse_identifier()?))
    }

    fn parse_identifier(&mut self) -> Result<ast::Identifier, Error> {
        if let Token::Identifier(name) = self.token()? {
            self.consume();
            Ok(ast::Identifier { name })
        } else {
            Err(SyntaxError::ExpectedToken(Token::Identifier(String::new()), self.token()?).into())
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

    fn peek(&mut self) -> Result<Token, Error> {
        self.token()?;
        Ok(self.lexer.peek()?)
    }

    fn consume(&mut self) {
        if self.token.is_none() {
            // Error in the parser logic not in user program
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
        // // println!("{:?} {:?}", self.token, loc);
        Ok(())
    }
}
