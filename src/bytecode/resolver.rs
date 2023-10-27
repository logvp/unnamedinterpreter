use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Ast, AstNode, Block, Expression, Statement},
    error::{Error, RuntimeError},
};

#[derive(Clone, Copy)]
enum LocalVariable {
    Local { is_const: bool },
    Captured { is_const: bool },
}

#[derive(Debug, Clone, Copy)]
pub(super) enum GlobalVariable {
    Constant,
    NotConstant,
    Unknown,
}

pub struct Scope {
    parent: usize,
    data: HashMap<Rc<str>, LocalVariable>,
}

#[derive(Default)]
pub struct ResolutionTable {
    local_scopes: Vec<Scope>,
    global_scope: HashMap<Rc<str>, GlobalVariable>,
}
impl ResolutionTable {
    fn parent_of(&self, scope: usize) -> usize {
        if scope == 0 {
            return 0;
        }
        self.local_scopes.get(scope - 1).map(|s| s.parent).unwrap()
    }

    fn make_scope(&mut self, content: Scope) -> usize {
        self.local_scopes.push(content);
        self.local_scopes.len()
    }

    fn make_declaration(
        &mut self,
        scope: usize,
        ident: Rc<str>,
        is_const: bool,
    ) -> Result<(), Error> {
        let redeclaration = if let Some(scope) = self.local_scopes.get_mut(scope - 1) {
            scope
                .data
                .insert(ident.clone(), LocalVariable::Local { is_const })
                .is_some()
        } else {
            match self.global_scope.insert(
                ident.clone(),
                if is_const {
                    GlobalVariable::Constant
                } else {
                    GlobalVariable::NotConstant
                },
            ) {
                Some(GlobalVariable::Unknown) => false,
                Some(_) => true,
                None => false,
            }
        };
        if redeclaration {
            return Err(RuntimeError::VariableRedeclaration(ident.to_string()).into());
        }
        Ok(())
    }

    fn make_assignment(&mut self, ident: Rc<str>, scope: usize) -> Result<(), Error> {
        if scope > 0 {
            match self.lookup_local(&ident, scope) {
                Ok(
                    LocalVariable::Local { is_const: false }
                    | LocalVariable::Captured { is_const: false },
                ) => {}
                Ok(
                    LocalVariable::Local { is_const: true }
                    | LocalVariable::Captured { is_const: true },
                ) => return Err(RuntimeError::ConstReassignment(ident.to_string()).into()),
                Err(_) => match self.global_scope.get(&ident) {
                    Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                    Some(GlobalVariable::Constant) => {
                        return Err(RuntimeError::ConstReassignment(ident.to_string()).into())
                    }
                    None => self.make_tentative_global(ident),
                },
            }
        } else {
            match self.global_scope.get(&ident) {
                Some(GlobalVariable::NotConstant) => {}
                Some(GlobalVariable::Constant) => {
                    return Err(RuntimeError::ConstReassignment(ident.to_string()).into())
                }
                Some(GlobalVariable::Unknown) | None => {
                    return Err(RuntimeError::UnknownIdentifier(ident.to_string()).into())
                }
            }
        }
        Ok(())
    }

    fn make_tentative_global(&mut self, ident: Rc<str>) {
        match self
            .global_scope
            .insert(Rc::clone(&ident), GlobalVariable::Unknown)
        {
            Some(GlobalVariable::Unknown) | None => {}
            Some(x) => panic!("Attempted to overwrite {:?} {ident} with Unknown", x),
        }
    }

    pub(super) fn lookup_global(&self, ident: &str) -> Result<GlobalVariable, Error> {
        match self.global_scope.get(ident) {
            Some(a @ (GlobalVariable::NotConstant | GlobalVariable::Constant)) => Ok(*a),
            Some(GlobalVariable::Unknown) | None => {
                return Err(RuntimeError::UnknownIdentifier(ident.to_string()).into())
            }
        }
    }

    pub(self) fn lookup_local(&self, ident: &str, scope: usize) -> Result<LocalVariable, Error> {
        let mut index = scope;
        while let Some(s) = self.local_scopes.get(index - 1) {
            if let Some(var) = s.data.get(ident) {
                return Ok(*var);
            }
            index = s.parent;
        }
        Err(RuntimeError::UnknownIdentifier(ident.to_string()).into())
    }
}

pub struct Resolver {
    current_scope: usize,
    scopes: ResolutionTable,
}
impl Resolver {
    pub fn new() -> Self {
        Resolver {
            current_scope: 0,
            scopes: Default::default(),
        }
    }

    pub fn get_table(&self) -> &ResolutionTable {
        &self.scopes
    }

    fn push_scope(&mut self) {
        self.current_scope = self.scopes.make_scope(Scope {
            data: Default::default(),
            parent: self.current_scope,
        });
    }

    fn pop_scope(&mut self) {
        // 'pop' old scope
        self.current_scope = self.scopes.parent_of(self.current_scope)
    }

    fn in_local_scope(&self) -> bool {
        self.current_scope > 0
    }

    pub fn define_globals(&mut self, identifiers: &[Rc<str>]) {
        for ident in identifiers.iter() {
            self.scopes
                .make_declaration(0, Rc::clone(ident), false)
                .unwrap()
        }
    }

    // fn get_local_scope(&self) -> Option<&Scope> {
    //     self.scopes.get_local_scope(self.current_scope)
    // }

    // fn get_local_scope_mut(&mut self) -> Option<&mut Scope> {
    //     self.scopes.get_local_scope_mut(self.current_scope)
    // }

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
                self.scopes
                    .make_assignment(name.clone(), self.current_scope)?;

                self.resolve_expr(expr)?;
            }
            Statement::Declaration(ident, expr, is_const) => {
                self.scopes.make_declaration(
                    self.current_scope,
                    Rc::clone(&ident.name),
                    *is_const,
                )?;
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
                if self.in_local_scope() {
                    if self.scopes.lookup_local(&name, self.current_scope).is_ok() {
                        return Ok(());
                    }
                    if self.scopes.lookup_global(&name).is_err() {
                        self.scopes.make_tentative_global(Rc::clone(name))
                    }
                } else {
                    self.scopes.lookup_global(name)?;
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
                for param in parameters.iter() {
                    self.scopes
                        .make_declaration(self.current_scope, param.name.clone(), false)?;
                    // parameters are variable by default
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
}
