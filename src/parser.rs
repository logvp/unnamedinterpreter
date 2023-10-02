use crate::ast;
use crate::error::{Error, SyntaxError};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
}
impl Parser {
    pub fn new(text: &str) -> Result<Self, Error> {
        Ok(Parser {
            lexer: Lexer::lex(text)?,
        })
    }

    pub fn gen_ast(&mut self) -> Result<ast::Ast, Error> {
        let mut ast: ast::Ast = Default::default();
        while !matches!(self.token(), Token::Eof) {
            let node = self.parse_ast_node()?;
            match node {
                ast::AstNode::Statement(_) => ast.push(node),
                ast::AstNode::Expression(_) => {
                    if !matches!(self.token(), Token::Eof) {
                        let ast::AstNode::Expression(expr) = node else {unreachable!()};
                        self.expect_semicolon()?;
                        ast.push(ast::AstNode::Statement(ast::Statement::Expression(expr)))
                    } else {
                        ast.push(node);
                        break;
                    }
                }
            }
        }

        self.expect_f(Token::Eof, |_| {
            SyntaxError::ExpressionMayOnlyComeAtEndIn(ast::Construct::AstNode).into()
        })?;

        Ok(ast)
    }

    fn parse_ast_node(&mut self) -> Result<ast::AstNode, Error> {
        if let Some(statement) = match self.token() {
            Token::Var => {
                self.consume();
                let identifier = self.parse_identifier(ast::Construct::Var)?;
                let equal = self.consume();
                if let Token::Equal = equal {
                    Some(ast::Statement::Declaration(
                        identifier,
                        self.parse_expression(ast::Construct::Var)?,
                        false,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        Token::Equal,
                        equal,
                        ast::Construct::Var,
                    )
                    .into());
                }
            }
            Token::Let => {
                self.consume();
                let identifier = self.parse_identifier(ast::Construct::Let)?;
                let equal = self.consume();
                if let Token::Equal = equal {
                    Some(ast::Statement::Declaration(
                        identifier,
                        self.parse_expression(ast::Construct::Let)?,
                        true,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        Token::Equal,
                        equal,
                        ast::Construct::Var,
                    )
                    .into());
                }
            }
            Token::Set => {
                self.consume();
                let lvalue = self.parse_lvalue(ast::Construct::Set)?;
                let equal = self.consume();
                if let Token::Equal = equal {
                    Some(ast::Statement::Assignment(
                        lvalue,
                        self.parse_expression(ast::Construct::Set)?,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        Token::Equal,
                        equal,
                        ast::Construct::Set,
                    )
                    .into());
                }
            }
            Token::Identifier(_) if matches!(self.peek(), &Token::ColonEqual) => {
                let identifier = self.parse_identifier(ast::Construct::SimpleDeclaration)?;
                // consume `:=`
                self.consume();
                Some(ast::Statement::Declaration(
                    identifier,
                    self.parse_expression(ast::Construct::SimpleDeclaration)?,
                    false,
                ))
            }
            // TODO: Only checks for identifier not for Lvalue
            Token::Identifier(_) if matches!(self.peek(), Token::Equal) => {
                let lvalue = self.parse_lvalue(ast::Construct::SimpleAssignment)?;
                // consume `=`
                self.consume();
                Some(ast::Statement::Assignment(
                    lvalue,
                    self.parse_expression(ast::Construct::SimpleAssignment)?,
                ))
            }
            _ => None,
        } {
            self.expect_semicolon()?;
            return Ok(ast::AstNode::Statement(statement));
        }
        // Let, Set, SimpleDeclaration, SimpleAssignment cases are covered
        // Statement::Expression and Expression need to be handled
        let expression = self.parse_expression(ast::Construct::Expression)?;

        if let Token::Semicolon = self.token() {
            self.consume();
            Ok(ast::AstNode::Statement(ast::Statement::Expression(
                expression,
            )))
        } else {
            Ok(ast::AstNode::Expression(expression))
        }
    }

    fn parse_expression(&mut self, ort: ast::Construct) -> Result<ast::Expression, Error> {
        Ok(match self.token() {
            Token::If => self.parse_if_expression()?,
            Token::While => {
                self.consume();
                let cond = self.parse_parenthetical_expression(ast::Construct::While)?;
                let body = self.parse_braced_block()?;
                ast::Expression::While(Box::new(cond), body)
            }
            Token::With => {
                self.consume();
                let object = self.parse_parenthetical_expression(ast::Construct::With)?;
                let body = self.parse_braced_block()?;
                ast::Expression::With(Box::new(object), body)
            }
            Token::New => {
                self.consume();
                let body = self.parse_braced_block()?;
                ast::Expression::New(body)
            }
            _ => {
                let lhs: ast::Term = self.parse_term(ort)?;
                match self.token() {
                    Token::Plus => {
                        self.consume();
                        ast::Expression::Add(lhs, Box::new(self.parse_expression(ort)?))
                    }
                    Token::Minus => {
                        self.consume();
                        ast::Expression::Subtract(lhs, Box::new(self.parse_expression(ort)?))
                    }
                    Token::EqualEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::Equal,
                        )
                    }
                    Token::LessEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::LessEqual,
                        )
                    }
                    Token::LeftAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::LessThan,
                        )
                    }
                    Token::GreaterEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::GreaterEqual,
                        )
                    }
                    Token::RightAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::GreaterThan,
                        )
                    }
                    Token::SlashEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::NotEqual,
                        )
                    }
                    _ => ast::Expression::Term(lhs),
                }
            }
        })
    }

    fn parse_term(&mut self, ort: ast::Construct) -> Result<ast::Term, Error> {
        let lhs: ast::Factor = self.parse_factor(ort)?;
        Ok(match self.token() {
            Token::Star => {
                self.consume();
                ast::Term::Multiply(lhs, Box::new(self.parse_term(ort)?))
            }
            Token::Slash => {
                self.consume();
                ast::Term::Divide(lhs, Box::new(self.parse_term(ort)?))
            }
            Token::PlusPlus => {
                self.consume();
                ast::Term::Concatenate(lhs, Box::new(self.parse_term(ort)?))
            }
            _ => ast::Term::Factor(lhs),
        })
    }

    fn parse_factor(&mut self, ort: ast::Construct) -> Result<ast::Factor, Error> {
        let factor = match self.token() {
            Token::Lambda => self.parse_lambda_expression()?,
            Token::LeftParen => ast::Factor::Expression(Box::new(
                self.parse_parenthetical_expression(ast::Construct::Expression)?,
            )),
            Token::LeftBrace => ast::Factor::Block(self.parse_braced_block()?),
            Token::Minus => {
                self.consume();
                ast::Factor::Negate(Box::new(self.parse_factor(ort)?))
            }
            Token::Literal(literal) => {
                let literal = literal.to_owned();
                self.consume();
                ast::Factor::Literal(literal)
            }
            Token::Identifier(name) => {
                let name = name.to_owned();
                self.consume();
                ast::Factor::Variable(ast::Identifier { name })
            }
            _ => Err(SyntaxError::UnexpectedTokenIn(self.consume(), ort))?,
        };
        if let Token::LeftParen = self.token() {
            self.parse_function_call(factor)
        } else {
            Ok(factor)
        }
    }

    fn parse_parenthetical_expression(
        &mut self,
        ort: ast::Construct,
    ) -> Result<ast::Expression, Error> {
        self.expect(Token::LeftParen, ort)?;

        let parenthesized = self.parse_expression(ort)?;

        self.expect(Token::RightParen, ort)?;

        Ok(parenthesized)
    }

    fn parse_braced_block(&mut self) -> Result<ast::Block, Error> {
        self.expect(Token::LeftBrace, ast::Construct::Block)?;
        let mut block: Vec<ast::AstNode> = Default::default();
        while !matches!(self.token(), Token::RightBrace) {
            let node = self.parse_ast_node()?;
            match node {
                ast::AstNode::Statement(_) => {
                    block.push(node);
                }
                ast::AstNode::Expression(_) => {
                    block.push(node);
                    break;
                }
            }
        }
        self.expect_f(Token::RightBrace, |_| {
            SyntaxError::ExpressionMayOnlyComeAtEndIn(ast::Construct::Block).into()
        })?;

        Ok(ast::Block(block))
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression, Error> {
        if let Token::If = self.token() {
            self.consume();
        } else {
            unreachable!()
        }
        let cond = self.parse_parenthetical_expression(ast::Construct::If)?;
        let body = self.parse_braced_block()?;
        let else_ = match (self.token(), self.peek()) {
            // else if
            (Token::Else, Token::If) => {
                self.consume();
                ast::Block(vec![ast::AstNode::Expression(self.parse_if_expression()?)])
            }
            (Token::Else, _) => {
                self.consume();
                self.parse_braced_block()?
            }
            _ => Default::default(),
        };
        Ok(ast::Expression::IfElse(Box::new(cond), body, else_))
    }

    fn parse_lambda_expression(&mut self) -> Result<ast::Factor, Error> {
        self.expect(Token::Lambda, ast::Construct::Lambda)?;
        let params = self.parse_lambda_params()?;
        let body = self.parse_braced_block()?;
        Ok(ast::Factor::Lambda(params, body))
    }

    fn parse_lambda_params(&mut self) -> Result<Vec<ast::Identifier>, Error> {
        const ORT: ast::Construct = ast::Construct::ParameterList;
        self.expect(Token::LeftParen, ORT)?;
        let mut params: Vec<ast::Identifier> = Default::default();
        if !matches!(self.token(), Token::RightParen) {
            params.push(self.parse_identifier(ORT)?);

            while matches!(self.token(), Token::Comma) {
                self.consume(); // ,
                params.push(self.parse_identifier(ORT)?);
            }
        }
        self.expect(Token::RightParen, ORT)?;
        Ok(params)
    }

    fn parse_function_call(&mut self, func: ast::Factor) -> Result<ast::Factor, Error> {
        let call = ast::Factor::FunctionCall(Box::new(func), self.parse_function_args()?);
        if let Token::LeftParen = self.token() {
            self.parse_function_call(call)
        } else {
            Ok(call)
        }
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, Error> {
        const ORT: ast::Construct = ast::Construct::FunctionCall;
        self.expect(Token::LeftParen, ORT)?;
        let mut args: Vec<ast::Expression> = Default::default();
        if !matches!(self.token(), Token::RightParen) && !self.eof() {
            args.push(self.parse_expression(ORT)?);

            while matches!(self.token(), Token::Comma) {
                self.consume(); // ,
                args.push(self.parse_expression(ORT)?);
            }
        }
        self.expect(Token::RightParen, ORT)?;
        Ok(args)
    }

    fn parse_lvalue(&mut self, ort: ast::Construct) -> Result<ast::Lvalue, Error> {
        Ok(ast::Lvalue::Identifier(self.parse_identifier(ort)?))
    }

    fn parse_identifier(&mut self, ort: ast::Construct) -> Result<ast::Identifier, Error> {
        let ident = self.consume();
        if let Token::Identifier(name) = ident {
            Ok(ast::Identifier { name })
        } else {
            Err(SyntaxError::ExpectedTokenIn(Token::Identifier(String::new()), ident, ort).into())
        }
    }

    fn expect_semicolon(&mut self) -> Result<Token, Error> {
        let token = self.lexer.next_token_nts();
        if matches!(token, Token::Semicolon) {
            Ok(token)
        } else {
            Err(
                SyntaxError::ExpectedTokenIn(Token::Semicolon, token, ast::Construct::Statement)
                    .into(),
            )
        }
    }

    fn expect_f(&mut self, expected_kind: Token, err: fn(Token) -> Error) -> Result<Token, Error> {
        let token = self.consume();
        if expected_kind.kind_eq(&token) {
            Ok(token)
        } else {
            Err(err(token))
        }
    }

    fn expect(&mut self, expected_kind: Token, ort: ast::Construct) -> Result<Token, Error> {
        let token = self.consume();
        if expected_kind.kind_eq(&token) {
            Ok(token)
        } else {
            Err(SyntaxError::ExpectedTokenIn(expected_kind, token, ort).into())
        }
    }

    fn token(&self) -> &Token {
        self.lexer.peek()
    }

    fn peek(&self) -> &Token {
        self.lexer.peek_over()
    }

    fn consume(&mut self) -> Token {
        self.lexer.next_token()
    }

    fn eof(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }
}
