use crate::{
    ast::{Ast, AstNode, BinaryOperator, Block, Expression, Literal, Statement, UnaryOperator},
    error::Error,
};

use super::resolver::ResolutionTable;

#[derive(Debug)]
enum Type {
    None,
    Boolean,
    Integer,
    String,
    Function(Box<[Type]>, Box<Type>),
    ExperimentalAny,
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::ExperimentalAny, _) | (_, Type::ExperimentalAny) => true,
            (Type::Function(from_a, to_a), Type::Function(from_b, to_b)) => {
                from_a == from_b && to_a == to_b
            }
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b),
        }
    }
}

#[derive(Debug)]
pub struct TypeError {}

pub struct TypeChecker {}

impl TypeChecker {
    fn new() -> Self {
        TypeChecker {}
    }

    pub fn check(ast: &Ast, _variables: &ResolutionTable) -> Result<(), TypeError> {
        let mut checker = TypeChecker::new();
        for node in ast.iter() {
            checker.check_node(node)?;
        }
        Ok(())
    }

    fn check_node(&mut self, node: &AstNode) -> Result<Type, TypeError> {
        match node {
            AstNode::Expression(e) => self.check_expr(e),
            AstNode::Statement(s) => match s {
                Statement::Declaration(_, e, _) => {
                    self.check_expr(e)?;
                    Ok(Type::None)
                }
                Statement::Assignment(_, e) => {
                    self.check_expr(e)?;
                    Ok(Type::None)
                }
                Statement::Expression(e) => self.check_expr(e),
            },
        }
    }

    fn check_binary(
        &mut self,
        op: BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Type, TypeError> {
        let lhs_t = self.check_expr(lhs)?;
        let rhs_t = self.check_expr(rhs)?;
        match op {
            BinaryOperator::Equal | BinaryOperator::NotEqual => Ok(Type::Boolean),
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual => {
                if lhs_t == Type::Integer && rhs_t == Type::Integer {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError {})
                }
            }
            BinaryOperator::Add
            | BinaryOperator::Divide
            | BinaryOperator::Multiply
            | BinaryOperator::Subtract => {
                if lhs_t == Type::Integer && rhs_t == Type::Integer {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError {})
                }
            }
            BinaryOperator::Concatenate => {
                if lhs_t == Type::String && rhs_t == Type::String {
                    Ok(Type::String)
                } else {
                    Err(TypeError {})
                }
            }
        }
    }

    fn check_unary(&mut self, op: UnaryOperator, e: &Expression) -> Result<Type, TypeError> {
        let e_t = self.check_expr(e)?;
        match op {
            UnaryOperator::Negate => {
                if e_t == Type::Integer {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError {})
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &Expression) -> Result<Type, TypeError> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Boolean(_) => Ok(Type::Boolean),
                Literal::Integer(_) => Ok(Type::Integer),
                Literal::String(_) => Ok(Type::String),
            },
            Expression::Variable(_) => Ok(Type::ExperimentalAny),
            Expression::Binary(op, lhs, rhs) => self.check_binary(*op, lhs, rhs),
            Expression::Lambda(params, body) => {
                let ret = self.check_block(body)?;
                Ok(Type::Function(
                    (0..params.len()).map(|_| Type::ExperimentalAny).collect(),
                    Box::new(ret),
                ))
            }
            Expression::While(cond, body) => {
                self.check_expr(cond)?;
                self.check_block(body)
            }
            Expression::IfElse(cond, body1, body2) => {
                self.check_expr(cond)?;
                let type_a = self.check_block(body1)?;
                let type_b = self.check_block(body2)?;
                if type_a == type_b {
                    Ok(type_a)
                } else {
                    panic!("if statement type mismatch")
                }
            }
            Expression::FunctionCall(func, args) => {
                let func_t = self.check_expr(func)?;
                let mut args_t = Vec::new();
                for arg in args.iter() {
                    args_t.push(self.check_expr(arg)?);
                }
                match func_t {
                    Type::ExperimentalAny => Ok(Type::ExperimentalAny),
                    Type::Function(from, to) if from.as_ref() == args_t.as_slice() => Ok(*to),
                    _ => panic!(),
                }
            }
            Expression::Unary(op, e) => self.check_unary(*op, e),
            Expression::Block(block) => self.check_block(block),
            e => todo!("{e:?}"),
        }
    }

    fn check_block(&mut self, block: &Block) -> Result<Type, TypeError> {
        let mut t = Type::None;
        let Block(nodes) = block;
        for node in nodes.iter() {
            t = self.check_node(node)?;
        }
        Ok(t)
    }
}
