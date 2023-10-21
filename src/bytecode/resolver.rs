use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Ast, AstNode, Block, Expression, Statement},
    error::{Error, RuntimeError},
};

enum LocalVariable {
    Local { is_const: bool },
    Captured { is_const: bool },
}

#[derive(Debug)]
enum GlobalVariable {
    Constant,
    NotConstant,
    Unknown,
}

pub struct Scope {
    parent: usize,
    data: HashMap<Rc<str>, LocalVariable>,
}
impl Scope {
    pub fn parent<'a>(&self, list: &'a [Scope]) -> Option<&'a Scope> {
        if self.parent > 0 {
            list.get(self.parent - 1)
        } else {
            None
        }
    }
}

pub struct Resolver {
    local_scopes: Vec<Scope>,
    current_scope: usize,
    global_scope: HashMap<Rc<str>, GlobalVariable>,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver {
            local_scopes: Vec::new(),
            current_scope: 0,
            global_scope: HashMap::new(),
        }
    }

    pub fn define_globals(&mut self, identifiers: &[Rc<str>]) {
        for ident in identifiers.iter() {
            self.global_scope
                .insert(ident.clone(), GlobalVariable::NotConstant);
        }
    }

    fn get_local_scope(&self) -> Option<&Scope> {
        if self.current_scope > 0 {
            Some(&self.local_scopes[self.current_scope - 1])
        } else {
            None
        }
    }

    fn get_local_scope_mut(&mut self) -> Option<&mut Scope> {
        if self.current_scope > 0 {
            Some(&mut self.local_scopes[self.current_scope - 1])
        } else {
            None
        }
    }

    fn lookup_local(&self, name: Rc<str>) -> Option<bool> {
        let mut scope = self.get_local_scope();
        while let Some(s) = scope {
            match s.data.get(&name) {
                Some(LocalVariable::Local { is_const })
                | Some(LocalVariable::Captured { is_const }) => return Some(*is_const),
                None => {
                    scope = if s.parent > 0 {
                        s.parent(&self.local_scopes)
                    } else {
                        break;
                    }
                }
            }
        }
        None
    }

    pub fn resolve(&mut self, ast: &Ast) -> Result<(), Error> {
        for node in ast.nodes.iter() {
            self.resolve_node(node)?;
        }
        // for (name, var) in self.global_scope.iter() {
        //     if let GlobalVariable::Unknown = var {
        //         // should not error in REPL mode, but should in file compilation
        //         // also predefined globals like the intrinsic functions shouldn't err
        //         // return Err(RuntimeError::UnknownIdentifier(name.to_string()).into());
        //     }
        // }
        Ok(())
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
                if self.get_local_scope().is_none() {
                    match self.global_scope.get(&name) {
                        Some(GlobalVariable::NotConstant) => {}
                        Some(GlobalVariable::Constant) => {
                            return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                        }
                        Some(GlobalVariable::Unknown) | None => {
                            return Err(RuntimeError::UnknownIdentifier(name.to_string()).into())
                        }
                    }
                } else {
                    match self.lookup_local(name.clone()) {
                        Some(false) => {}
                        Some(true) => {
                            return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                        }
                        None => match self.global_scope.get(&name) {
                            Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                            Some(GlobalVariable::Constant) => {
                                return Err(RuntimeError::ConstReassignment(name.to_string()).into())
                            }
                            None => {
                                self.global_scope
                                    .insert(name.clone(), GlobalVariable::Unknown);
                            }
                        },
                    }
                }
                self.resolve_expr(expr)?;
            }
            Statement::Declaration(ident, expr, is_const) => {
                let redeclaration = if let Some(scope) = self.get_local_scope_mut() {
                    scope
                        .data
                        .insert(
                            ident.name.clone(),
                            LocalVariable::Local {
                                is_const: *is_const,
                            },
                        )
                        .is_some()
                } else {
                    self.global_scope
                        .insert(
                            ident.name.clone(),
                            if *is_const {
                                GlobalVariable::Constant
                            } else {
                                GlobalVariable::NotConstant
                            },
                        )
                        .is_some()
                };
                if redeclaration {
                    return Err(RuntimeError::VariableRedeclaration(ident.name.to_string()).into());
                }
                self.resolve_expr(expr)?;
            }
            Statement::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expression) -> Result<(), Error> {
        match expr {
            Expression::Binary(_op, lhs, rhs) => {
                self.resolve_expr(lhs)?;
                self.resolve_expr(rhs)?;
            }
            Expression::Unary(_op, lhs) => {
                self.resolve_expr(lhs)?;
            }
            Expression::Literal(_literal) => {}
            Expression::Variable(ident) => {
                let name = &ident.name;
                if self.get_local_scope().is_none() {
                    match self.global_scope.get(name) {
                        Some(GlobalVariable::NotConstant) => {}
                        Some(GlobalVariable::Constant) => {}
                        Some(GlobalVariable::Unknown) | None => {
                            return Err(RuntimeError::UnknownIdentifier(name.to_string()).into())
                        }
                    }
                } else {
                    match self.lookup_local(name.clone()) {
                        Some(false) => {}
                        Some(true) => {}
                        None => match self.global_scope.get(name) {
                            Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                            Some(GlobalVariable::Constant) => {}
                            None => {
                                self.global_scope
                                    .insert(name.clone(), GlobalVariable::Unknown);
                            }
                        },
                    }
                }
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
            Expression::Lambda(parameters, body) => {
                self.push_scope();
                let scope = self.get_local_scope_mut().unwrap();
                for param in parameters.iter() {
                    scope
                        .data
                        .insert(param.name.clone(), LocalVariable::Local { is_const: false });
                }
                self.resolve_block(body)?;
                self.pop_scope();
            }
            x => todo!("Resolving {:?} is not implemented yet", x),
        }
        Ok(())
    }

    fn resolve_block(&mut self, block: &Block) -> Result<(), Error> {
        let Block(nodes) = block;
        self.push_scope();
        for node in nodes.iter() {
            self.resolve_node(node)?;
        }
        self.pop_scope();
        Ok(())
    }

    fn push_scope(&mut self) {
        self.local_scopes.push(Scope {
            data: Default::default(),
            parent: self.current_scope,
        });
        self.current_scope = self.local_scopes.len();
    }

    fn pop_scope(&mut self) {
        // 'pop' old scope
        self.current_scope = self.get_local_scope().unwrap().parent;
    }
}
