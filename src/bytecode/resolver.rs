use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{AstNode, Block, Expression, Identifier, Statement},
    error::{Error, RuntimeError},
};

enum LocalVariable {
    Local { is_const: bool },
    Captured { is_const: bool },
}

enum GlobalVariable {
    Constant,
    NotConstant,
    Unknown,
}

pub struct Resolver {
    local_scopes: Vec<HashMap<Rc<str>, LocalVariable>>,
    global_scope: HashMap<Rc<str>, GlobalVariable>,
}

impl Resolver {
    fn lookup_local(&self, name: Rc<str>) -> Option<bool> {
        todo!()
    }

    fn resolve_node(&mut self, node: &AstNode) -> Result<(), Error> {
        match node {
            AstNode::Expression(expr) => self.resolve_expr(expr),
            AstNode::Statement(stmt) => self.resolve_stmt(stmt),
        }
    }

    fn resolve_stmt(&mut self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Assignment(lvalue, expr) => {
                let name = lvalue.name().unwrap();
                if self.local_scopes.is_empty() {
                    match self.global_scope.get(&name) {
                        Some(GlobalVariable::NotConstant) => {}
                        Some(GlobalVariable::Constant) => {
                            return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                        }
                        Some(GlobalVariable::Unknown) | None => {
                            return Err(RuntimeError::UnknownIdentifier(name.to_string()).into())
                        }
                    }
                }
                match self.lookup_local(name.clone()) {
                    Some(false) => {}
                    Some(true) => {
                        return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                    }
                    None => match self.global_scope.get(&name) {
                        Some(GlobalVariable::Constant) => {
                            return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                        }
                        Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                        None => {
                            self.global_scope
                                .insert(name.clone(), GlobalVariable::Unknown);
                        }
                    },
                }
                self.resolve_expr(expr)?;
            }
            Statement::Declaration(ident, expr, is_const) => {
                if let Some(scope) = self.local_scopes.last_mut() {
                    let last = scope.insert(
                        ident.name.clone(),
                        LocalVariable::Local {
                            is_const: *is_const,
                        },
                    );
                    if last.is_some() {
                        return Err(
                            RuntimeError::VariableRedeclaration(ident.name.to_string()).into()
                        );
                    }
                } else {
                    let last = self.global_scope.insert(
                        ident.name.clone(),
                        if *is_const {
                            GlobalVariable::Constant
                        } else {
                            GlobalVariable::NotConstant
                        },
                    );
                    if last.is_some() {
                        return Err(
                            RuntimeError::VariableRedeclaration(ident.name.to_string()).into()
                        );
                    }
                }
            }
            Statement::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), Error> {
        match expr {
            Expression::Binary(op, lhs, rhs) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)?;
            }
            Expression::Unary(op, lhs) => {
                self.resolve_expr(lhs)?;
            }
            Expression::Literal(_literal) => {}
            Expression::Variable(_ident) => {
                todo!()
            }
            Expression::Block(block) => {
                self.resolve_block(block)?;
            }
            Expression::IfElse(expr, if_block, else_block) => {
                self.resolve_expr(expr)?;
                self.resolve_block(if_block)?;
                self.resolve_block(else_block)?;
            }
            Expression::While(expr, body) => {
                self.resolve_expr(expr)?;
                self.resolve_block(body)?;
            }
            Expression::FunctionCall(fun, arguments) => {
                self.resolve_expr(fun)?;
                for arg in arguments.iter() {
                    self.resolve_expr(arg)?;
                }
            }
            Expression::Lambda(_parameters, body) => {
                self.resolve_block(body)?;
            }
            x => todo!("Resolving {:?} is not implemented yet", x),
        }
        Ok(())
    }

    fn resolve_block(&mut self, block: &Block) -> Result<(), Error> {
        let Block(nodes) = block;
        for node in nodes.iter() {
            self.resolve_node(node)?;
        }
        Ok(())
    }
}
