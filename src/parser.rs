use std::rc::Rc;

use crate::ast;
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

    pub fn gen_ast(&mut self) -> Result<ast::Ast, Error> {
        let mut ast: ast::Ast = Default::default();
        while !matches!(self.token(), TokenKind::Eof) {
            let node = self.parse_ast_node()?;
            match node {
                ast::AstNode::Statement(_) => ast.push(node),
                ast::AstNode::Expression(expr) if !matches!(self.token(), TokenKind::Eof) => {
                    self.expect_semicolon()?;
                    ast.push(ast::AstNode::Statement(ast::Statement::Expression(expr)))
                }
                ast::AstNode::Expression(_) => {
                    ast.push(node);
                    break;
                }
            }
        }

        if let TokenKind::Eof = self.consume() {
            Ok(ast)
        } else {
            Err(SyntaxError::ExpressionMayOnlyComeAtEndIn(
                ast::Construct::AstNode,
                self.loc.clone(),
            )
            .into())
        }
    }

    fn parse_ast_node(&mut self) -> Result<ast::AstNode, Error> {
        if let Some(statement) = match self.token() {
            TokenKind::Var => {
                self.consume();
                let identifier = self.parse_identifier(ast::Construct::Var)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(ast::Statement::Declaration(
                        identifier,
                        self.parse_expression(ast::Construct::Var)?,
                        false,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        ast::Construct::Var,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Let => {
                self.consume();
                let identifier = self.parse_identifier(ast::Construct::Let)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(ast::Statement::Declaration(
                        identifier,
                        self.parse_expression(ast::Construct::Let)?,
                        true,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        ast::Construct::Var,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Set => {
                self.consume();
                let lvalue = self.parse_lvalue(ast::Construct::Set)?;
                let equal = self.consume();
                if let TokenKind::Equal = equal {
                    Some(ast::Statement::Assignment(
                        lvalue,
                        self.parse_expression(ast::Construct::Set)?,
                    ))
                } else {
                    return Err(SyntaxError::ExpectedTokenIn(
                        TokenKind::Equal,
                        equal,
                        ast::Construct::Set,
                        self.loc.clone(),
                    )
                    .into());
                }
            }
            TokenKind::Identifier(_) if matches!(self.peek(), &TokenKind::ColonEqual) => {
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
            TokenKind::Identifier(_) if matches!(self.peek(), TokenKind::Equal) => {
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

        if let TokenKind::Semicolon = self.token() {
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
            TokenKind::If => self.parse_if_expression()?,
            TokenKind::While => {
                self.consume();
                let cond = self.parse_parenthetical_expression(ast::Construct::WhileCondition)?;
                let body = self.parse_braced_block(ast::Construct::WhileBody)?;
                ast::Expression::While(Box::new(cond), body)
            }
            TokenKind::With => {
                self.consume();
                let object = self.parse_parenthetical_expression(ast::Construct::With)?;
                let body = self.parse_braced_block(ast::Construct::With)?;
                ast::Expression::With(Box::new(object), body)
            }
            TokenKind::New => {
                self.consume();
                let body = self.parse_braced_block(ast::Construct::NewBlock)?;
                ast::Expression::New(body)
            }
            _ => {
                let lhs: ast::Term = self.parse_term(ort)?;
                match self.token() {
                    TokenKind::Plus => {
                        self.consume();
                        ast::Expression::Add(lhs, Box::new(self.parse_expression(ort)?))
                    }
                    TokenKind::Minus => {
                        self.consume();
                        ast::Expression::Subtract(lhs, Box::new(self.parse_expression(ort)?))
                    }
                    TokenKind::EqualEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::Equal,
                        )
                    }
                    TokenKind::LessEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::LessEqual,
                        )
                    }
                    TokenKind::LeftAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::LessThan,
                        )
                    }
                    TokenKind::GreaterEqual => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::GreaterEqual,
                        )
                    }
                    TokenKind::RightAngle => {
                        self.consume();
                        ast::Expression::Compare(
                            lhs,
                            Box::new(self.parse_expression(ort)?),
                            ast::Comparison::GreaterThan,
                        )
                    }
                    TokenKind::SlashEqual => {
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
            TokenKind::Star => {
                self.consume();
                ast::Term::Multiply(lhs, Box::new(self.parse_term(ort)?))
            }
            TokenKind::Slash => {
                self.consume();
                ast::Term::Divide(lhs, Box::new(self.parse_term(ort)?))
            }
            TokenKind::PlusPlus => {
                self.consume();
                ast::Term::Concatenate(lhs, Box::new(self.parse_term(ort)?))
            }
            _ => ast::Term::Factor(lhs),
        })
    }

    fn parse_factor(&mut self, ort: ast::Construct) -> Result<ast::Factor, Error> {
        let factor = match self.token() {
            TokenKind::Lambda => self.parse_lambda_expression()?,
            TokenKind::LeftParen => ast::Factor::Expression(Box::new(
                self.parse_parenthetical_expression(ast::Construct::Expression)?,
            )),
            TokenKind::LeftBrace => {
                ast::Factor::Block(self.parse_braced_block(ast::Construct::BlockScope)?)
            }
            TokenKind::Minus => {
                self.consume();
                ast::Factor::Negate(Box::new(self.parse_factor(ort)?))
            }
            TokenKind::Literal(literal) => {
                let literal = literal.to_owned();
                self.consume();
                ast::Factor::Literal(literal)
            }
            TokenKind::Identifier(name) => {
                let name = name.to_owned();
                self.consume();
                ast::Factor::Variable(ast::Identifier { name })
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

    fn parse_parenthetical_expression(
        &mut self,
        ort: ast::Construct,
    ) -> Result<ast::Expression, Error> {
        self.expect(TokenKind::LeftParen, ort)?;

        let parenthesized = self.parse_expression(ort)?;

        self.expect(TokenKind::RightParen, ort)?;

        Ok(parenthesized)
    }

    fn parse_braced_block(&mut self, ort: ast::Construct) -> Result<ast::Block, Error> {
        self.expect(TokenKind::LeftBrace, ort)?;
        let mut block: Vec<ast::AstNode> = Default::default();
        while !matches!(self.token(), TokenKind::RightBrace) {
            let node = self.parse_ast_node()?;
            match node {
                ast::AstNode::Statement(_) => block.push(node),
                ast::AstNode::Expression(expr)
                    if !matches!(self.token(), TokenKind::RightBrace) =>
                {
                    self.expect_semicolon()?;
                    block.push(ast::AstNode::Statement(ast::Statement::Expression(expr)))
                }
                ast::AstNode::Expression(_) => {
                    block.push(node);
                    break;
                }
            }
        }

        if let TokenKind::RightBrace = self.consume() {
            Ok(ast::Block(block))
        } else {
            Err(SyntaxError::ExpressionMayOnlyComeAtEndIn(ort, self.loc.clone()).into())
        }
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression, Error> {
        if let TokenKind::If = self.token() {
            self.consume();
        } else {
            unreachable!()
        }
        let cond = self.parse_parenthetical_expression(ast::Construct::IfCondition)?;
        let body = self.parse_braced_block(ast::Construct::IfBody)?;
        let else_ = match (self.token(), self.peek()) {
            // else if
            (TokenKind::Else, TokenKind::If) => {
                self.consume();
                ast::Block(vec![ast::AstNode::Expression(self.parse_if_expression()?)])
            }
            (TokenKind::Else, _) => {
                self.consume();
                self.parse_braced_block(ast::Construct::ElseBody)?
            }
            _ => Default::default(),
        };
        Ok(ast::Expression::IfElse(Box::new(cond), body, else_))
    }

    fn parse_lambda_expression(&mut self) -> Result<ast::Factor, Error> {
        self.expect(TokenKind::Lambda, ast::Construct::Lambda)?;
        let params = self.parse_lambda_params()?;
        let body = self.parse_braced_block(ast::Construct::LambdaBody)?;
        Ok(ast::Factor::Lambda(params, body))
    }

    fn parse_lambda_params(&mut self) -> Result<Vec<ast::Identifier>, Error> {
        const ORT: ast::Construct = ast::Construct::ParameterList;
        self.expect(TokenKind::LeftParen, ORT)?;
        let mut params: Vec<ast::Identifier> = Default::default();
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

    fn parse_function_call(&mut self, func: ast::Factor) -> Result<ast::Factor, Error> {
        let call = ast::Factor::FunctionCall(Box::new(func), self.parse_function_args()?);
        if let TokenKind::LeftParen = self.token() {
            self.parse_function_call(call)
        } else {
            Ok(call)
        }
    }

    fn parse_function_args(&mut self) -> Result<Vec<ast::Expression>, Error> {
        const ORT: ast::Construct = ast::Construct::FunctionCall;
        self.expect(TokenKind::LeftParen, ORT)?;
        let mut args: Vec<ast::Expression> = Default::default();
        if !matches!(self.token(), TokenKind::RightParen) && !self.eof() {
            args.push(self.parse_expression(ORT)?);

            while matches!(self.token(), TokenKind::Comma) {
                self.consume(); // ,
                args.push(self.parse_expression(ORT)?);
            }
        }
        self.expect(TokenKind::RightParen, ORT)?;
        Ok(args)
    }

    fn parse_lvalue(&mut self, ort: ast::Construct) -> Result<ast::Lvalue, Error> {
        Ok(ast::Lvalue::Identifier(self.parse_identifier(ort)?))
    }

    fn parse_identifier(&mut self, ort: ast::Construct) -> Result<ast::Identifier, Error> {
        let ident = self.consume();
        if let TokenKind::Identifier(name) = ident {
            Ok(ast::Identifier { name })
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
            Err(SyntaxError::ExpectedTokenIn(
                TokenKind::Semicolon,
                kind,
                ast::Construct::Statement,
                loc,
            )
            .into())
        }
    }

    fn expect(
        &mut self,
        expected_kind: TokenKind,
        ort: ast::Construct,
    ) -> Result<TokenKind, Error> {
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
