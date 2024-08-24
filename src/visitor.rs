use crate::ast::*;

pub trait AstVisitor {
    type Bad;
    type Good: Default;

    fn visit_ast(&mut self, ast: &Ast) -> Result<Self::Good, Self::Bad> {
        let mut ret = Default::default();
        for node in ast.iter() {
            ret = self.visit_node(node)?;
        }
        Ok(ret)
    }

    fn visit_node(&mut self, node: &AstNode) -> Result<Self::Good, Self::Bad> {
        match node {
            AstNode::Expression(e) => self.visit_expr(e),
            AstNode::Statement(s) => self.visit_statement(s),
        }
    }

    fn visit_statement(&mut self, s: &Statement) -> Result<Self::Good, Self::Bad> {
        match s {
            Statement::Declaration(id, e, is_const) => self.visit_declaration(id, e, *is_const),
            Statement::Assignment(id, e) => self.visit_assignment(id, e),
            Statement::Expression(e) => self.visit_expr(e),
        }
    }

    fn visit_declaration(
        &mut self,
        id: &Identifier,
        e: &Expression,
        is_const: bool,
    ) -> Result<Self::Good, Self::Bad>;

    fn visit_assignment(&mut self, id: &Lvalue, e: &Expression) -> Result<Self::Good, Self::Bad>;

    fn visit_expr(&mut self, expr: &Expression) -> Result<Self::Good, Self::Bad> {
        match expr {
            Expression::Literal(lit) => self.visit_literal(lit),
            Expression::Variable(var) => self.visit_variable(var),
            Expression::Binary(op, lhs, rhs) => self.visit_binary(*op, lhs, rhs),
            Expression::Lambda(params, body) => self.visit_lambda(params, body),
            Expression::While(cond, body) => self.visit_while(cond, body),
            Expression::IfElse(cond, if_true, if_false) => {
                self.visit_if_else(cond, if_true, if_false)
            }
            Expression::FunctionCall(func, args) => self.visit_function_call(func, args),
            Expression::Unary(op, e) => self.visit_unary(*op, e),
            Expression::Block(block) => self.visit_block(block),
        }
    }

    fn visit_literal(&mut self, lit: &Literal) -> Result<Self::Good, Self::Bad>;

    fn visit_variable(&mut self, var: &Identifier) -> Result<Self::Good, Self::Bad>;

    fn visit_while(&mut self, cond: &Expression, body: &Block) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(cond)?;
        self.visit_block(body)
    }

    fn visit_lambda(
        &mut self,
        params: &[Identifier],
        body: &Block,
    ) -> Result<Self::Good, Self::Bad>;

    fn visit_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Self::Good, Self::Bad>;

    fn visit_if_else(
        &mut self,
        cond: &Expression,
        if_true: &Block,
        if_false: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(cond)?;
        self.visit_block(if_true)?;
        self.visit_block(if_false)
    }

    fn visit_binary(
        &mut self,
        op: BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Self::Good, Self::Bad>;

    fn visit_unary(&mut self, op: UnaryOperator, e: &Expression) -> Result<Self::Good, Self::Bad>;

    fn visit_block(&mut self, block: &Block) -> Result<Self::Good, Self::Bad> {
        let mut ret = Default::default();
        for node in block.iter() {
            ret = self.visit_node(node)?;
        }
        Ok(ret)
    }
}
