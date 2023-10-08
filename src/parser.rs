use std::rc::Rc;

use crate::ast::{
    Ast, AstNode, BinaryOperator, Block, Construct, Expression, Identifier, Lvalue, PrefixOperator,
    Statement,
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
        self.parse_expression_rec(0, ort)
    }

    // Expression parser based on https://github.com/matklad/minipratt
    fn parse_expression_rec(&mut self, min_pow: u8, ort: Construct) -> Result<Expression, Error> {
        let mut lhs = match self.token() {
            TokenKind::Literal(_) => {
                let TokenKind::Literal(x) = self.consume() else {unreachable!() };
                Expression::Literal(x)
            }
            TokenKind::Identifier(_) => Expression::Variable(self.parse_identifier(ort)?),
            TokenKind::LeftParen => self.parse_parenthetical_expression(ort)?,
            token if Self::convert_prefix_operator(&token).is_some() => {
                let unop = Self::convert_prefix_operator(&self.consume()).unwrap();
                let r_pow = Self::prefix_power(unop);
                let rhs = self.parse_expression_rec(r_pow, ort)?;
                Expression::Unary(unop, Box::new(rhs))
            }
            TokenKind::If => self.parse_if_expression()?,
            TokenKind::While => self.parse_while_expression()?,
            TokenKind::With => self.parse_with_expression()?,
            TokenKind::New => self.parse_new_expression()?,
            TokenKind::Lambda => self.parse_lambda_expression()?,
            TokenKind::LeftBrace => {
                Expression::Block(self.parse_braced_block(Construct::BlockScope)?)
            }
            _ => {
                return Err(
                    SyntaxError::UnexpectedTokenIn(self.consume(), ort, self.loc.clone()).into(),
                )
            }
        };

        loop {
            if let TokenKind::LeftParen = self.token() {
                let args = self.parse_function_args()?;
                lhs = Expression::FunctionCall(Box::new(lhs), args);
                continue;
            }

            let Some(op) = Self::convert_infix_operator(self.token()) else { break; };

            let (l_pow, r_pow) = Self::infix_power(op);
            if l_pow < min_pow {
                break;
            }

            self.consume(); // chomp op
            let rhs = self.parse_expression_rec(r_pow, ort)?;
            lhs = Expression::Binary(op, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn infix_power(op: BinaryOperator) -> (u8, u8) {
        match op {
            BinaryOperator::Add | BinaryOperator::Subtract => (5, 6),
            BinaryOperator::Multiply | BinaryOperator::Divide => (9, 10),
            BinaryOperator::Equal | BinaryOperator::NotEqual => (1, 2),
            BinaryOperator::GreaterEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::LessThan => (3, 4),
            BinaryOperator::Concatenate => todo!(),
        }
    }

    fn prefix_power(op: PrefixOperator) -> u8 {
        match op {
            PrefixOperator::Negate => 15,
        }
    }

    fn convert_infix_operator(token: &TokenKind) -> Option<BinaryOperator> {
        Some(match token {
            TokenKind::RightAngle => BinaryOperator::GreaterThan,
            TokenKind::LeftAngle => BinaryOperator::LessThan,
            TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenKind::LessEqual => BinaryOperator::LessEqual,
            TokenKind::EqualEqual => BinaryOperator::Equal,
            TokenKind::SlashEqual => BinaryOperator::NotEqual,
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Star => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::PlusPlus => BinaryOperator::Concatenate,
            _ => return None,
        })
    }

    fn convert_prefix_operator(token: &TokenKind) -> Option<PrefixOperator> {
        Some(match token {
            TokenKind::Minus => PrefixOperator::Negate,
            _ => return None,
        })
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

    fn parse_while_expression(&mut self) -> Result<Expression, Error> {
        self.expect(TokenKind::While, Construct::WhileCondition)?;
        let cond = self.parse_parenthetical_expression(Construct::WhileCondition)?;
        let body = self.parse_braced_block(Construct::WhileBody)?;
        Ok(Expression::While(Box::new(cond), body))
    }

    fn parse_with_expression(&mut self) -> Result<Expression, Error> {
        self.expect(TokenKind::With, Construct::With)?;
        let object = self.parse_parenthetical_expression(Construct::With)?;
        let body = self.parse_braced_block(Construct::With)?;
        Ok(Expression::With(Box::new(object), body))
    }

    fn parse_new_expression(&mut self) -> Result<Expression, Error> {
        self.expect(TokenKind::New, Construct::NewBlock)?;
        let body = self.parse_braced_block(Construct::NewBlock)?;
        Ok(Expression::New(body))
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
