use std::rc::Rc;

use crate::ast::{
    Ast, AstNode, BinaryOperator, Block, Construct, Expression, Identifier, Lvalue, Statement,
    UnaryOperator,
};
use crate::error::{Error, Loc, SyntaxError};
use crate::lexer::{Lexer, Token, TokenKind};

pub struct Parser {
    lexer: Lexer,
    loc: Loc,
}
impl Parser {
    pub fn new(text: &str, filename: Option<Rc<str>>) -> Result<Self, Error> {
        Ok(Parser {
            lexer: Lexer::lex(text, filename.clone())?,
            loc: Loc::new(filename),
        })
    }

    pub fn gen_ast(&mut self) -> Result<Ast, Error> {
        let mut ast = Ast::default();
        while !matches!(self.token(), TokenKind::Eof) {
            let node = self.parse_ast_node()?;
            match node {
                AstNode::Statement(_) => ast.push(node),
                AstNode::Expression(expr) if !matches!(self.token(), TokenKind::Eof) => {
                    self.expect_semicolon()?;
                    ast.push(AstNode::Statement(Statement::Expression(expr)))
                }
                AstNode::Expression(_) => {
                    ast.push(node);
                    break;
                }
            }
        }

        if let TokenKind::Eof = self.consume() {
            Ok(ast)
        } else {
            Err(
                SyntaxError::ExpressionMayOnlyComeAtEndIn(Construct::AstNode, self.loc.clone())
                    .into(),
            )
        }
    }

    fn parse_ast_node(&mut self) -> Result<AstNode, Error> {
        if let Some(statement) = match self.token() {
            TokenKind::Var => {
                self.consume();
                let identifier = self.parse_identifier(Construct::Var)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(Statement::Declaration(
                        identifier,
                        self.parse_expression(Construct::Var)?,
                        false,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        Construct::Var,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Let => {
                self.consume();
                let identifier = self.parse_identifier(Construct::Let)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(Statement::Declaration(
                        identifier,
                        self.parse_expression(Construct::Let)?,
                        true,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        Construct::Var,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Set => {
                self.consume();
                let lvalue = self.parse_lvalue(Construct::Set)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(Statement::Assignment(
                        lvalue,
                        self.parse_expression(Construct::Set)?,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        Construct::Set,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Identifier(_) if matches!(self.peek(), &TokenKind::ColonEqual) => {
                let identifier = self.parse_identifier(Construct::SimpleDeclaration)?;
                // consume `:=`
                self.consume();
                Some(Statement::Declaration(
                    identifier,
                    self.parse_expression(Construct::SimpleDeclaration)?,
                    false,
                ))
            }
            // TODO: Only checks for identifier not for Lvalue
            TokenKind::Identifier(_) if matches!(self.peek(), TokenKind::Equal) => {
                let lvalue = self.parse_lvalue(Construct::SimpleAssignment)?;
                // consume `=`
                self.consume();
                Some(Statement::Assignment(
                    lvalue,
                    self.parse_expression(Construct::SimpleAssignment)?,
                ))
            }
            _ => None,
        } {
            self.expect_semicolon()?;
            return Ok(AstNode::Statement(statement));
        }
        // Let, Set, SimpleDeclaration, SimpleAssignment cases are covered
        // Statement::Expression and Expression need to be handled
        let expression = self.parse_expression(Construct::Expression)?;

        if let TokenKind::Semicolon = self.token() {
            self.consume();
            Ok(AstNode::Statement(Statement::Expression(expression)))
        } else {
            Ok(AstNode::Expression(expression))
        }
    }

    fn parse_expression(&mut self, ort: Construct) -> Result<Expression, Error> {
        Ok(match self.token() {
            TokenKind::If => self.parse_if_expression()?,
            TokenKind::While => {
                self.consume();
                let cond = self.parse_parenthetical_expression(Construct::WhileCondition)?;
                let body = self.parse_braced_block(Construct::WhileBody)?;
                Expression::While(Box::new(cond), body)
            }
            TokenKind::With => {
                self.consume();
                let object = self.parse_parenthetical_expression(Construct::With)?;
                let body = self.parse_braced_block(Construct::With)?;
                Expression::With(Box::new(object), body)
            }
            TokenKind::New => {
                self.consume();
                let body = self.parse_braced_block(Construct::NewBlock)?;
                Expression::New(body)
            }
            _ => {
                let lhs: Expression = self.parse_term(ort)?;
                match self.token() {
                    TokenKind::Plus => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::Add,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::Minus => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::Subtract,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::EqualEqual => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::Equal,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::LessEqual => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::LessEqual,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::LeftAngle => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::LessThan,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::GreaterEqual => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::GreaterEqual,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::RightAngle => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::GreaterThan,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    TokenKind::SlashEqual => {
                        self.consume();
                        Expression::Binary(
                            BinaryOperator::NotEqual,
                            Box::new(lhs),
                            Box::new(self.parse_expression(ort)?),
                        )
                    }
                    _ => lhs,
                }
            }
        })
    }

    fn parse_term(&mut self, ort: Construct) -> Result<Expression, Error> {
        let lhs = self.parse_factor(ort)?;
        Ok(match self.token() {
            TokenKind::Star => {
                self.consume();
                Expression::Binary(
                    BinaryOperator::Multiply,
                    Box::new(lhs),
                    Box::new(self.parse_term(ort)?),
                )
            }
            TokenKind::Slash => {
                self.consume();
                Expression::Binary(
                    BinaryOperator::Divide,
                    Box::new(lhs),
                    Box::new(self.parse_term(ort)?),
                )
            }
            TokenKind::PlusPlus => {
                self.consume();
                Expression::Binary(
                    BinaryOperator::Concatenate,
                    Box::new(lhs),
                    Box::new(self.parse_term(ort)?),
                )
            }
            _ => lhs,
        })
    }

    fn parse_factor(&mut self, ort: Construct) -> Result<Expression, Error> {
        let factor = match self.token() {
            TokenKind::Lambda => self.parse_lambda_expression()?,
            TokenKind::LeftParen => self.parse_parenthetical_expression(Construct::Expression)?,

            TokenKind::LeftBrace => {
                Expression::Block(self.parse_braced_block(Construct::BlockScope)?)
            }
            TokenKind::Minus => {
                self.consume();
                Expression::Unary(UnaryOperator::Negate, Box::new(self.parse_factor(ort)?))
            }
            TokenKind::Literal(literal) => {
                let literal = literal.to_owned();
                self.consume();
                Expression::Literal(literal)
            }
            TokenKind::Identifier(name) => {
                let name = name.to_owned();
                self.consume();
                Expression::Variable(Identifier { name })
            }
            _ => {
                let tok = self.consume();
                return Err(SyntaxError::UnexpectedTokenIn(tok, ort, self.loc.clone()).into());
            }
        };
        if let TokenKind::LeftParen = self.token() {
            self.parse_function_call(factor)
        } else {
            Ok(factor)
        }
    }

    fn parse_parenthetical_expression(&mut self, ort: Construct) -> Result<Expression, Error> {
        self.expect(TokenKind::LeftParen, ort)?;

        let parenthesized = self.parse_expression(ort)?;

        self.expect(TokenKind::RightParen, ort)?;

        Ok(parenthesized)
    }

    fn parse_braced_block(&mut self, ort: Construct) -> Result<Block, Error> {
        self.expect(TokenKind::LeftBrace, ort)?;
        let mut block: Vec<AstNode> = Default::default();
        while !matches!(self.token(), TokenKind::RightBrace) {
            let node = self.parse_ast_node()?;
            match node {
                AstNode::Statement(_) => block.push(node),
                AstNode::Expression(expr) if !matches!(self.token(), TokenKind::RightBrace) => {
                    self.expect_semicolon()?;
                    block.push(AstNode::Statement(Statement::Expression(expr)))
                }
                AstNode::Expression(_) => {
                    block.push(node);
                    break;
                }
            }
        }

        if let TokenKind::RightBrace = self.consume() {
            Ok(Block(block.into()))
        } else {
            Err(SyntaxError::ExpressionMayOnlyComeAtEndIn(ort, self.loc.clone()).into())
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, Error> {
        if let TokenKind::If = self.token() {
            self.consume();
        } else {
            unreachable!()
        }
        let cond = self.parse_parenthetical_expression(Construct::IfCondition)?;
        let body = self.parse_braced_block(Construct::IfBody)?;
        let else_ = match (self.token(), self.peek()) {
            // else if
            (TokenKind::Else, TokenKind::If) => {
                self.consume();
                Block(Rc::new([AstNode::Expression(self.parse_if_expression()?)]))
            }
            (TokenKind::Else, _) => {
                self.consume();
                self.parse_braced_block(Construct::ElseBody)?
            }
            _ => Block::empty(),
        };
        Ok(Expression::IfElse(Box::new(cond), body, else_))
    }

    fn parse_lambda_expression(&mut self) -> Result<Expression, Error> {
        self.expect(TokenKind::Lambda, Construct::Lambda)?;
        let params = self.parse_lambda_params()?;
        let body = self.parse_braced_block(Construct::LambdaBody)?;
        Ok(Expression::Lambda(params.into(), body))
    }

    fn parse_lambda_params(&mut self) -> Result<Vec<Identifier>, Error> {
        const ORT: Construct = Construct::ParameterList;
        self.expect(TokenKind::LeftParen, ORT)?;
        let mut params: Vec<Identifier> = Default::default();
        if !matches!(self.token(), TokenKind::RightParen) {
            params.push(self.parse_identifier(ORT)?);

            while matches!(self.token(), TokenKind::Comma) {
                self.consume(); // ,
                params.push(self.parse_identifier(ORT)?);
            }
        }
        self.expect(TokenKind::RightParen, ORT)?;
        Ok(params)
    }

    fn parse_function_call(&mut self, func: Expression) -> Result<Expression, Error> {
        let call = Expression::FunctionCall(Box::new(func), self.parse_function_args()?);
        if let TokenKind::LeftParen = self.token() {
            self.parse_function_call(call)
        } else {
            Ok(call)
        }
    }

    fn parse_function_args(&mut self) -> Result<Rc<[Expression]>, Error> {
        const ORT: Construct = Construct::FunctionCall;
        self.expect(TokenKind::LeftParen, ORT)?;
        let mut args: Vec<Expression> = Default::default();
        if !matches!(self.token(), TokenKind::RightParen) && !self.eof() {
            args.push(self.parse_expression(ORT)?);

            while matches!(self.token(), TokenKind::Comma) {
                self.consume(); // ,
                args.push(self.parse_expression(ORT)?);
            }
        }
        self.expect(TokenKind::RightParen, ORT)?;
        Ok(args.into())
    }

    fn parse_lvalue(&mut self, ort: Construct) -> Result<Lvalue, Error> {
        Ok(Lvalue::Identifier(self.parse_identifier(ort)?))
    }

    fn parse_identifier(&mut self, ort: Construct) -> Result<Identifier, Error> {
        let ident = self.consume();
        if let TokenKind::Identifier(name) = ident {
            Ok(Identifier { name })
        } else {
            Err(SyntaxError::ExpectedTokenIn(
                TokenKind::Identifier(String::new()),
                ident,
                ort,
                self.loc.clone(),
            )
            .into())
        }
    }

    fn expect_semicolon(&mut self) -> Result<(), Error> {
        let Token { kind, loc } = self.lexer.next_token_allow_newline();
        if matches!(kind, TokenKind::Semicolon | TokenKind::Newline) {
            Ok(())
        } else {
            Err(
                SyntaxError::ExpectedTokenIn(TokenKind::Semicolon, kind, Construct::Statement, loc)
                    .into(),
            )
        }
    }

    fn expect(&mut self, expected_kind: TokenKind, ort: Construct) -> Result<TokenKind, Error> {
        let token = self.consume();
        if expected_kind.kind_eq(&token) {
            Ok(token)
        } else {
            Err(SyntaxError::ExpectedTokenIn(expected_kind, token, ort, self.loc.clone()).into())
        }
    }

    fn token(&self) -> &TokenKind {
        self.lexer.peek()
    }

    fn peek(&self) -> &TokenKind {
        self.lexer.peek_over()
    }

    fn consume(&mut self) -> TokenKind {
        let Token { kind, loc } = self.lexer.next_token();
        self.loc = loc;
        kind
    }

    fn eof(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }
}
