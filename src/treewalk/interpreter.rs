use std::borrow::Borrow;
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::interpreter::{Interpreter, RuntimeType};
use crate::parser::Parser;
use crate::visitor::AstVisitor;

use super::runtime::{Context, FunctionType, Lambda, Object, Variable};
use super::RuntimeValue;

pub struct TreeWalkInterpreter {
    context: Rc<Context>,
}
impl Interpreter for TreeWalkInterpreter {
    type ReplReturn = RuntimeValue;

    fn new() -> Self {
        TreeWalkInterpreter {
            context: Rc::new(Context::init_global()),
        }
    }

    fn interpret(
        &mut self,
        text: &str,
        filename: Option<Rc<str>>,
    ) -> Vec<Result<RuntimeValue, Error>> {
        let mut ret = Vec::new();
        match Parser::gen_ast(text, filename) {
            Ok(ast) => {
                for node in ast.iter() {
                    ret.push(self.visit_node(node));
                }
            }
            Err(e) => ret.push(Err(e)),
        }
        ret
    }
}

impl AstVisitor for TreeWalkInterpreter {
    type Good = RuntimeValue;
    type Bad = Error;

    fn visit_variable(&mut self, var: &Identifier) -> Result<Self::Good, Self::Bad> {
        match self.context.get(&var.name) {
            Some(Variable { val, .. }) => Ok(val),
            None => Err(RuntimeError::UnknownIdentifier(var.name.to_string()).into()), // Allocation
        }
    }

    fn visit_literal(&mut self, lit: &Literal) -> Result<Self::Good, Self::Bad> {
        match lit {
            Literal::Integer(int) => Ok(RuntimeValue::Integer(*int)),
            Literal::String(string) => Ok(RuntimeValue::String(string.to_string())), // Allocation
            Literal::Boolean(boolean) => Ok(RuntimeValue::Boolean(*boolean)),
        }
    }

    fn visit_block(&mut self, block: &Block) -> Result<Self::Good, Self::Bad> {
        let mut ctx = Self {
            context: Rc::new(Context::new(self.context.clone())),
        };
        let Block(nodes) = block;
        let mut ret = RuntimeValue::None;
        for node in nodes.iter() {
            ret = ctx.visit_node(node)?;
        }
        Ok(ret)
    }

    fn visit_binary(
        &mut self,
        op: BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        Ok(match op {
            BinaryOperator::Equal => {
                RuntimeValue::Boolean(self.visit_expr(lhs)? == self.visit_expr(rhs)?)
            }
            BinaryOperator::NotEqual => {
                RuntimeValue::Boolean(self.visit_expr(lhs)? != self.visit_expr(rhs)?)
            }
            BinaryOperator::LessThan => {
                RuntimeValue::Boolean(self.visit_expr(lhs)?.int()? < self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::LessEqual => {
                RuntimeValue::Boolean(self.visit_expr(lhs)?.int()? <= self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::GreaterThan => {
                RuntimeValue::Boolean(self.visit_expr(lhs)?.int()? > self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::GreaterEqual => {
                RuntimeValue::Boolean(self.visit_expr(lhs)?.int()? >= self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::Add => {
                RuntimeValue::Integer(self.visit_expr(lhs)?.int()? + self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::Subtract => {
                RuntimeValue::Integer(self.visit_expr(lhs)?.int()? - self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::Multiply => {
                RuntimeValue::Integer(self.visit_expr(lhs)?.int()? * self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::Divide => {
                RuntimeValue::Integer(self.visit_expr(lhs)?.int()? / self.visit_expr(rhs)?.int()?)
            }
            BinaryOperator::Concatenate => RuntimeValue::String(format!(
                "{}{}",
                self.visit_expr(lhs)?.string()?,
                self.visit_expr(rhs)?.string()?
            )),
        })
    }

    fn visit_unary(&mut self, op: UnaryOperator, e: &Expression) -> Result<Self::Good, Self::Bad> {
        match op {
            UnaryOperator::Negate => Ok(RuntimeValue::Integer(-self.visit_expr(e)?.int()?)),
        }
    }

    fn visit_statement(&mut self, s: &Statement) -> Result<Self::Good, Self::Bad> {
        match s {
            Statement::Declaration(lhs, rhs, is_const) => {
                self.visit_declaration(lhs, rhs, *is_const)
            }
            Statement::Assignment(lhs, rhs) => self.visit_assignment(lhs, rhs),
            Statement::Expression(expr) => {
                self.visit_expr(expr)?;
                Ok(RuntimeValue::None)
            }
        }
    }

    fn visit_declaration(
        &mut self,
        lhs: &Identifier,
        rhs: &Expression,
        is_const: bool,
    ) -> Result<Self::Good, Self::Bad> {
        if !self.context.contains_in_scope(&lhs.name) {
            let value = self.visit_expr(rhs)?;
            self.context.declare(lhs.name.to_string(), value, is_const);
        } else {
            Err(RuntimeError::VariableRedeclaration(lhs.name.to_string()))?
        }
        Ok(RuntimeValue::None)
    }

    fn visit_assignment(
        &mut self,
        lhs: &Lvalue,
        rhs: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        if self.context.contains(lhs.name().unwrap().as_ref()) {
            let value = self.visit_expr(rhs)?;
            self.context
                .update(lhs.name().unwrap().to_string(), value)?;
        } else {
            Err(RuntimeError::UnknownIdentifier(
                lhs.name().unwrap().to_string(),
            ))?
        }
        Ok(RuntimeValue::None)
    }

    fn visit_if_else(
        &mut self,
        cond: &Expression,
        if_true: &Block,
        if_false: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        if self.visit_expr(cond)?.boolean()? {
            self.visit_block(if_true)
        } else {
            self.visit_block(if_false)
        }
    }

    fn visit_while(&mut self, cond: &Expression, body: &Block) -> Result<Self::Good, Self::Bad> {
        let mut ret = RuntimeValue::None;
        while self.visit_expr(cond)?.boolean()? {
            ret = self.visit_block(body)?;
        }
        Ok(ret)
    }

    fn visit_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Self::Good, Self::Bad> {
        let func = self.visit_expr(func)?;
        let args = args
            .iter()
            .map(|x| self.visit_expr(x))
            .collect::<Result<Vec<RuntimeValue>, _>>()?;
        match func {
            RuntimeValue::Function(f) => {
                match f.borrow() {
                    FunctionType::Lambda(Lambda {
                        parent_scope,
                        parameters,
                        body,
                    }) => {
                        let scope = Context::new(Rc::clone(parent_scope));
                        if parameters.len() != args.len() {
                            Err(RuntimeError::ExpectedArgumentsFound(
                                parameters.len(),
                                args.len(),
                            ))?
                        }
                        // Bind arguments to parameter names
                        for (ident, val) in parameters.iter().zip(args.into_iter()) {
                            scope.declare(ident.name.to_string(), val, false);
                        }

                        let mut ctx = Self {
                            context: Rc::new(scope),
                        };

                        ctx.visit_block(body)
                    }
                    FunctionType::Intrinsic(f) => f.call(args),
                }
            }
            x => Err(RuntimeError::ExpectedButFound(RuntimeType::Function, x.get_type()).into()),
        }
    }

    fn visit_lambda(
        &mut self,
        params: &[Identifier],
        body: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        Ok(RuntimeValue::Function(Rc::new(FunctionType::Lambda(
            Lambda {
                parent_scope: self.context.clone(),
                parameters: Rc::from(params), // Allocation
                body: body.clone(),
            },
        ))))
    }
}
