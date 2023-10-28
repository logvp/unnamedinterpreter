use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{Ast, AstNode, Block, Expression, Statement},
    error::{Error, RuntimeError},
};

#[derive(Clone, Copy)]
pub(super) enum LocalVariable {
    Local {
        is_const: bool,
        index: Option<usize>,
    },
    Captured {
        is_const: bool,
    },
}

#[derive(Debug, Clone, Copy)]
pub(super) enum GlobalVariable {
    Constant,
    NotConstant,
    Unknown,
}

pub struct Scope {
    num_locals: usize,
    parent: usize,
    data: HashMap<Rc<str>, LocalVariable>,
}

#[derive(Default)]
pub(super) struct ResolutionTable {
    local_scopes: Vec<Scope>,
    global_scope: HashMap<Rc<str>, GlobalVariable>,
}
impl ResolutionTable {
    pub fn parent_of(&self, scope: usize) -> usize {
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
        let redeclaration = if scope > 0 {
            let scope = self.local_scopes.get_mut(scope - 1).unwrap();
            scope
                .data
                .insert(
                    ident.clone(),
                    LocalVariable::Local {
                        is_const,
                        index: None,
                    },
                )
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

    fn make_assignment(
        &mut self,
        ident: Rc<str>,
        scope: usize,
        in_a_closure: bool,
    ) -> Result<(), Error> {
        if scope > 0 {
            match self.lookup_local(&ident, scope) {
                Some(
                    LocalVariable::Local {
                        is_const: false, ..
                    }
                    | LocalVariable::Captured { is_const: false },
                ) => {}
                Some(
                    LocalVariable::Local { is_const: true, .. }
                    | LocalVariable::Captured { is_const: true },
                ) => return Err(RuntimeError::ConstReassignment(ident.to_string()).into()),
                None => match self.global_scope.get(&ident) {
                    Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                    Some(GlobalVariable::Constant) => {
                        return Err(RuntimeError::ConstReassignment(ident.to_string()).into())
                    }
                    None => self.found_unknown_variable(ident, in_a_closure)?,
                },
            }
        } else {
            match self.global_scope.get(&ident) {
                Some(GlobalVariable::NotConstant) => {}
                Some(GlobalVariable::Constant) => {
                    return Err(RuntimeError::ConstReassignment(ident.to_string()).into())
                }
                Some(GlobalVariable::Unknown) | None => {
                    self.found_unknown_variable(ident, in_a_closure)?
                }
            }
        }
        Ok(())
    }

    fn found_unknown_variable(&mut self, ident: Rc<str>, in_a_closure: bool) -> Result<(), Error> {
        if in_a_closure {
            // it could be a global that has not been defined yet, but will be before this closure is called
            match self
                .global_scope
                .insert(Rc::clone(&ident), GlobalVariable::Unknown)
            {
                Some(GlobalVariable::Unknown) | None => Ok(()),
                Some(x) => panic!("Attempted to overwrite {:?} {ident} with Unknown", x),
            }
        } else {
            return Err(RuntimeError::UnknownIdentifier(ident.to_string()).into());
        }
    }

    fn resolve_local_addresses(&mut self, scope: usize) {
        let mut i = 0;
        let scope = &mut self.local_scopes[scope - 1];
        for var in scope.data.values_mut() {
            if let LocalVariable::Local { index, .. } = var {
                *index = Some(i);
                i += 1;
            }
        }
        scope.num_locals = i;
    }

    pub fn get_num_locals_in_scope(&self, scope: usize) -> usize {
        self.local_scopes[scope - 1].num_locals
    }

    pub fn lookup_global(&self, ident: &str) -> Result<GlobalVariable, Error> {
        match self.global_scope.get(ident) {
            Some(a @ (GlobalVariable::NotConstant | GlobalVariable::Constant)) => Ok(*a),
            Some(GlobalVariable::Unknown) | None => {
                Err(RuntimeError::UnknownIdentifier(ident.to_string()).into())
            }
        }
    }

    pub fn lookup_local(&self, ident: &str, scope: usize) -> Option<LocalVariable> {
        let mut index = scope;
        while index > 0 {
            let s = self.local_scopes.get(index - 1).unwrap();
            if let Some(var) = s.data.get(ident) {
                return Some(*var);
            }
            index = s.parent;
        }
        None
    }

    pub fn lookup_local_in_scope(&self, ident: &str, scope: usize) -> Option<LocalVariable> {
        if scope == 0 {
            return None;
        }
        self.local_scopes[scope - 1].data.get(ident).cloned()
    }
}

pub(super) struct Resolver {
    current_scope: usize,
    in_a_closure: usize,
    scopes: ResolutionTable,
}
impl Resolver {
    pub fn new() -> Self {
        Resolver {
            current_scope: 0,
            in_a_closure: 0,
            scopes: Default::default(),
        }
    }

    pub fn get_table(&self) -> &ResolutionTable {
        &self.scopes
    }

    fn push_scope(&mut self) {
        self.current_scope = self.scopes.make_scope(Scope {
            num_locals: 0,
            data: Default::default(),
            parent: self.current_scope,
        });
    }

    fn pop_scope(&mut self) {
        // 'pop' old scope
        self.scopes.resolve_local_addresses(self.current_scope);
        self.current_scope = self.scopes.parent_of(self.current_scope);
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
                    .make_assignment(name, self.current_scope, self.in_a_closure > 0)?;

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
                    if self
                        .scopes
                        .lookup_local(&name, self.current_scope)
                        .is_some()
                    {
                        return Ok(());
                    }
                    if self.scopes.lookup_global(&name).is_err() {
                        self.scopes
                            .found_unknown_variable(Rc::clone(name), self.in_a_closure > 0)?
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
                for arg in arguments.iter() {
                    self.resolve_expr(arg)?;
                }
                self.resolve_expr(fun)?;
            }
            Expression::Lambda(parameters, body) => {
                self.push_scope();
                for param in parameters.iter() {
                    // parameters are mutable by default
                    self.scopes
                        .make_declaration(self.current_scope, param.name.clone(), false)?;
                }
                self.in_a_closure += 1;
                self.resolve_block(body)?;
                self.in_a_closure -= 1;
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
