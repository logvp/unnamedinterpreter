use crate::{
    ast::{Ast, BinaryOperator, Block, Expression, Identifier, Literal, Lvalue, UnaryOperator},
    visitor::AstVisitor,
};

#[derive(Debug)]
#[derive(Default)]
pub enum Type {
    #[default]
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
pub enum TypeError {
    ExpectedButFound(Type, Type),
    UnopExpectsButFound(UnaryOperator, Type, Type),
    BinopExpectsButFound(BinaryOperator, Type, Type, Type),
}

pub struct TypeChecker {}

impl TypeChecker {
    fn new() -> Self {
        TypeChecker {}
    }

    pub fn check(ast: &Ast) -> Result<(), TypeError> {
        let mut checker = TypeChecker::new();
        checker.visit_ast(ast)?;
        Ok(())
    }

    fn expect(actual: Type, expected: Type) -> Result<Type, TypeError> {
        if actual == expected {
            Ok(actual)
        } else {
            Err(TypeError::ExpectedButFound(expected, actual))
        }
    }
}

impl AstVisitor for TypeChecker {
    type Good = Type;
    type Bad = TypeError;

    fn visit_declaration(
        &mut self,
        _id: &Identifier,
        e: &Expression,
        _: bool,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(e)?;
        Ok(Type::None)
    }

    fn visit_assignment(&mut self, _id: &Lvalue, e: &Expression) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(e)?;
        Ok(Type::None)
    }

    fn visit_binary(
        &mut self,
        op: BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Type, TypeError> {
        let lhs_t = self.visit_expr(lhs)?;
        let rhs_t = self.visit_expr(rhs)?;
        match op {
            BinaryOperator::Equal | BinaryOperator::NotEqual => Ok(Type::Boolean),
            BinaryOperator::LessThan
            | BinaryOperator::GreaterThan
            | BinaryOperator::LessEqual
            | BinaryOperator::GreaterEqual => {
                if lhs_t == Type::Integer && rhs_t == Type::Integer {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeError::BinopExpectsButFound(
                        op,
                        Type::Integer,
                        lhs_t,
                        rhs_t,
                    ))
                }
            }
            BinaryOperator::Add
            | BinaryOperator::Divide
            | BinaryOperator::Multiply
            | BinaryOperator::Subtract => {
                if lhs_t == Type::Integer && rhs_t == Type::Integer {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError::BinopExpectsButFound(
                        op,
                        Type::Integer,
                        lhs_t,
                        rhs_t,
                    ))
                }
            }
            BinaryOperator::Concatenate => {
                if lhs_t == Type::String && rhs_t == Type::String {
                    Ok(Type::String)
                } else {
                    Err(TypeError::BinopExpectsButFound(
                        op,
                        Type::String,
                        lhs_t,
                        rhs_t,
                    ))
                }
            }
        }
    }

    fn visit_unary(&mut self, op: UnaryOperator, e: &Expression) -> Result<Type, TypeError> {
        let e_t = self.visit_expr(e)?;
        match op {
            UnaryOperator::Negate => {
                if e_t == Type::Integer {
                    Ok(Type::Integer)
                } else {
                    Err(TypeError::UnopExpectsButFound(op, Type::Integer, e_t))
                }
            }
        }
    }

    fn visit_lambda(
        &mut self,
        params: &[crate::ast::Identifier],
        body: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        let ret = self.visit_block(body)?;
        Ok(Type::Function(
            (0..params.len()).map(|_| Type::ExperimentalAny).collect(),
            Box::new(ret),
        ))
    }

    fn visit_while(&mut self, cond: &Expression, body: &Block) -> Result<Self::Good, Self::Bad> {
        Self::expect(self.visit_expr(cond)?, Type::Boolean)?;
        self.visit_block(body)
    }

    fn visit_if_else(
        &mut self,
        cond: &Expression,
        if_true: &Block,
        if_false: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        Self::expect(self.visit_expr(cond)?, Type::Boolean)?;
        let type_a = self.visit_block(if_true)?;
        let type_b = self.visit_block(if_false)?;
        if type_a == type_b {
            Ok(type_a)
        } else {
            Err(TypeError::ExpectedButFound(type_a, type_b))
        }
    }

    fn visit_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Self::Good, Self::Bad> {
        let func_t = self.visit_expr(func)?;
        let mut args_t = Vec::new();
        for arg in args.iter() {
            args_t.push(self.visit_expr(arg)?);
        }
        match func_t {
            Type::ExperimentalAny => Ok(Type::ExperimentalAny),
            Type::Function(from, to) if from.as_ref() == args_t.as_slice() => Ok(*to),
            _ => Err(TypeError::ExpectedButFound(
                Type::Function(args_t.into(), Box::new(Type::ExperimentalAny)),
                func_t,
            )),
        }
    }

    fn visit_literal(&mut self, lit: &Literal) -> Result<Self::Good, Self::Bad> {
        match lit {
            Literal::Boolean(_) => Ok(Type::Boolean),
            Literal::Integer(_) => Ok(Type::Integer),
            Literal::String(_) => Ok(Type::String),
        }
    }

    fn visit_variable(&mut self, _var: &Identifier) -> Result<Self::Good, Self::Bad> {
        Ok(Type::ExperimentalAny)
    }

    fn visit_block(&mut self, block: &Block) -> Result<Type, TypeError> {
        let mut t = Type::None;
        let Block(nodes) = block;
        for node in nodes.iter() {
            t = self.visit_node(node)?;
        }
        Ok(t)
    }
}
