use crate::{
    ast::{Ast, AstNode, BinaryOperator, Block, Expression, Literal, Statement, UnaryOperator},
    visitor::AstVisitor,
};

pub struct Printer {
    indent: usize,
    indent_str: &'static str,
}

impl Printer {
    pub fn print(ast: &Ast) -> std::string::String {
        let mut printer = Printer::new();
        printer.visit_ast(ast).unwrap()
    }

    fn new() -> Self {
        Printer {
            indent: 0,
            indent_str: "    ",
        }
    }

    fn newline(&self) -> std::string::String {
        "\n".to_string() + &self.indent_str.repeat(self.indent)
    }
}

impl AstVisitor for Printer {
    type Good = std::string::String;
    type Bad = ();

    fn visit_node(&mut self, node: &crate::ast::AstNode) -> Result<Self::Good, Self::Bad> {
        let mut buf = self.newline();
        buf += &match node {
            AstNode::Expression(e) => self.visit_expr(e)?,
            AstNode::Statement(s) => self.visit_statement(s)?,
        };
        Ok(buf)
    }

    fn visit_statement(&mut self, s: &Statement) -> Result<Self::Good, Self::Bad> {
        let mut buf = match s {
            Statement::Declaration(id, e, is_const) => self.visit_declaration(id, e, *is_const)?,
            Statement::Assignment(id, e) => self.visit_assignment(id, e)?,
            Statement::Expression(e) => self.visit_expr(e)?,
        };
        buf += ";";
        Ok(buf)
    }

    fn visit_assignment(
        &mut self,
        id: &crate::ast::Lvalue,
        e: &crate::ast::Expression,
    ) -> Result<Self::Good, Self::Bad> {
        let mut lhs = id.name().unwrap().to_string();
        lhs += " = ";
        lhs += &self.visit_expr(e)?;
        Ok(lhs)
    }

    fn visit_declaration(
        &mut self,
        id: &crate::ast::Identifier,
        e: &crate::ast::Expression,
        is_const: bool,
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = (if is_const { "let " } else { "var " }).to_string();
        buf += id.name.as_ref();
        buf += " = ";
        buf += &self.visit_expr(e)?;
        Ok(buf)
    }

    fn visit_literal(&mut self, lit: &crate::ast::Literal) -> Result<Self::Good, Self::Bad> {
        Ok(match lit {
            Literal::Boolean(b) => b.to_string(),
            Literal::Integer(i) => i.to_string(),
            Literal::String(s) => format!("\"{s}\""),
        })
    }

    fn visit_variable(&mut self, var: &crate::ast::Identifier) -> Result<Self::Good, Self::Bad> {
        Ok(var.name.to_string())
    }

    fn visit_lambda(
        &mut self,
        params: &[crate::ast::Identifier],
        body: &crate::ast::Block,
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = "lambda (".to_string();
        let mut first = true;
        for param in params {
            if !first {
                buf += ", ";
            }
            buf += &self.visit_variable(param)?;
            first = false;
        }
        buf += ") ";
        buf += &self.visit_block(body)?;
        Ok(buf)
    }

    fn visit_function_call(
        &mut self,
        func: &crate::ast::Expression,
        args: &[crate::ast::Expression],
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = self.visit_expr(func)?; // Is there a case where parens are needed here?
        buf += "(";
        let mut first = true;
        for arg in args {
            if !first {
                buf += ", ";
            }
            buf += &self.visit_expr(arg)?;
            first = false;
        }
        buf += ")";
        Ok(buf)
    }

    fn visit_binary(
        &mut self,
        op: crate::ast::BinaryOperator,
        lhs: &crate::ast::Expression,
        rhs: &crate::ast::Expression,
    ) -> Result<Self::Good, Self::Bad> {
        // parenthesis are needed if this operator precedence is lower than the outer operator precedence
        let mut buf = "(".to_string();
        buf += &self.visit_expr(lhs)?;
        buf += match op {
            BinaryOperator::Equal => " == ",
            BinaryOperator::NotEqual => " /= ",
            BinaryOperator::LessThan => " < ",
            BinaryOperator::LessEqual => " <= ",
            BinaryOperator::GreaterThan => " > ",
            BinaryOperator::GreaterEqual => " >= ",
            BinaryOperator::Add => " + ",
            BinaryOperator::Subtract => " - ",
            BinaryOperator::Multiply => " * ",
            BinaryOperator::Divide => " / ",
            BinaryOperator::Concatenate => " ++ ",
        };
        buf += &self.visit_expr(rhs)?;
        buf += ")";
        Ok(buf)
    }

    fn visit_unary(
        &mut self,
        op: crate::ast::UnaryOperator,
        e: &crate::ast::Expression,
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = "(".to_string();
        buf += match op {
            UnaryOperator::Negate => "-",
        };
        buf += &self.visit_expr(e)?;
        buf += ")";
        Ok(buf)
    }

    fn visit_block(&mut self, block: &crate::ast::Block) -> Result<Self::Good, Self::Bad> {
        self.indent += 1;
        let mut buf = "{".to_string();
        let Block(nodes) = block;
        for node in nodes.iter() {
            buf += &self.visit_node(node)?;
        }
        self.indent -= 1;
        buf += &self.newline();
        buf += "}";
        Ok(buf)
    }

    fn visit_ast(&mut self, ast: &crate::ast::Ast) -> Result<Self::Good, Self::Bad> {
        let mut buf = String::new();
        for node in ast.iter() {
            buf += &self.visit_node(node)?;
        }
        Ok(buf)
    }

    fn visit_if_else(
        &mut self,
        cond: &crate::ast::Expression,
        if_true: &Block,
        if_false: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = "if (".to_string();
        buf += &self.visit_expr(cond)?;
        buf += ") ";
        buf += &self.visit_block(if_true)?;
        match if_false.0.as_ref() {
            [] => {}
            [AstNode::Expression(Expression::IfElse(elif_cond, elif_true, elif_false))] => {
                buf += " else ";
                buf += &self.visit_if_else(elif_cond, elif_true, elif_false)?;
            }
            _ => {
                buf += " else ";
                buf += &self.visit_block(if_false)?;
            }
        }
        Ok(buf)
    }

    fn visit_while(
        &mut self,
        cond: &crate::ast::Expression,
        body: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        let mut buf = "while (".to_string();
        buf += &self.visit_expr(cond)?;
        buf += ") ";
        buf += &self.visit_block(body)?;
        Ok(buf)
    }
}
