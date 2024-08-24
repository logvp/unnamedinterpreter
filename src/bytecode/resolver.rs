use std::collections::HashMap;

use crate::{
    ast::{Ast, Block, Expression},
    error::{Error, RuntimeError},
    visitor::AstVisitor,
    Symbol,
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
    data: HashMap<Symbol, LocalVariable>,
}

#[derive(Default)]
pub(super) struct ResolutionTable {
    local_scopes: Vec<Scope>,
    global_scope: HashMap<Symbol, GlobalVariable>,
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
        ident: Symbol,
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
            return Err(RuntimeError::VariableRedeclaration(ident.clone()).into());
        }
        Ok(())
    }

    fn make_assignment(
        &mut self,
        ident: Symbol,
        scope: usize,
        closure_boundary: usize,
    ) -> Result<(), Error> {
        if scope > 0 {
            match self.get_local_and_capture(&ident, scope, closure_boundary) {
                Some(
                    LocalVariable::Local {
                        is_const: false, ..
                    }
                    | LocalVariable::Captured { is_const: false },
                ) => {}
                Some(
                    LocalVariable::Local { is_const: true, .. }
                    | LocalVariable::Captured { is_const: true },
                ) => return Err(RuntimeError::ConstReassignment(ident.clone()).into()),
                None => match self.global_scope.get(&ident) {
                    Some(GlobalVariable::NotConstant) | Some(GlobalVariable::Unknown) => {}
                    Some(GlobalVariable::Constant) => {
                        return Err(RuntimeError::ConstReassignment(ident.clone()).into())
                    }
                    None => self.found_unknown_variable(ident, closure_boundary > 0)?,
                },
            }
        } else {
            match self.global_scope.get(&ident) {
                Some(GlobalVariable::NotConstant) => {}
                Some(GlobalVariable::Constant) => {
                    return Err(RuntimeError::ConstReassignment(ident.clone()).into())
                }
                Some(GlobalVariable::Unknown) | None => {
                    self.found_unknown_variable(ident, closure_boundary > 0)?
                }
            }
        }
        Ok(())
    }

    fn found_unknown_variable(&mut self, ident: Symbol, in_a_closure: bool) -> Result<(), Error> {
        if in_a_closure {
            // it could be a global that has not been defined yet, but will be before this closure is called
            match self
                .global_scope
                .insert(ident.clone(), GlobalVariable::Unknown)
            {
                Some(GlobalVariable::Unknown) | None => Ok(()),
                Some(x) => panic!("Attempted to overwrite {:?} {ident} with Unknown", x),
            }
        } else {
            Err(RuntimeError::UnknownIdentifier(ident.clone()).into())
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

    pub fn lookup_global(&self, ident: &Symbol) -> Result<GlobalVariable, Error> {
        match self.global_scope.get(ident) {
            Some(a @ (GlobalVariable::NotConstant | GlobalVariable::Constant)) => Ok(*a),
            Some(GlobalVariable::Unknown) | None => {
                Err(RuntimeError::UnknownIdentifier(ident.clone()).into())
            }
        }
    }

    pub fn get_local_and_capture(
        &mut self,
        ident: &Symbol,
        mut scope: usize,
        closure_boundary: usize,
    ) -> Option<LocalVariable> {
        while scope > 0 {
            let s = self.local_scopes.get_mut(scope - 1).unwrap();
            if let Some(var) = s.data.get_mut(ident) {
                if scope < closure_boundary {
                    if let LocalVariable::Local { is_const, .. } = *var {
                        *var = LocalVariable::Captured { is_const }
                    }
                }
                return Some(*var);
            }
            scope = s.parent;
        }
        None
    }

    pub fn lookup_local_in_scope(&self, ident: &Symbol, scope: usize) -> Option<LocalVariable> {
        if scope == 0 {
            return None;
        }
        self.local_scopes[scope - 1].data.get(ident).cloned()
    }
}

pub(super) struct Resolver {
    current_scope: usize,
    closure_boundary: usize,
    scopes: ResolutionTable,
}
impl Resolver {
    pub fn new() -> Self {
        Resolver {
            current_scope: 0,
            closure_boundary: 0,
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

    pub fn define_globals(&mut self, identifiers: &[Symbol]) {
        for ident in identifiers.iter() {
            self.scopes
                .make_declaration(0, ident.clone(), false)
                .unwrap()
        }
    }

    pub fn resolve(&mut self, ast: &Ast) -> Result<(), Error> {
        self.visit_ast(ast)
        // for (name, var) in self.global_scope.iter() {
        //     if let GlobalVariable::Unknown = var {
        //         // should not error in REPL mode, but should in file compilation
        //         // also predefined globals like the intrinsic functions shouldn't err
        //         // return Err(RuntimeError::UnknownIdentifier(name.clone()).into());
        //     }
        // }
        // Ok(())
    }
}

impl AstVisitor for Resolver {
    type Good = ();
    type Bad = Error;

    fn visit_block(&mut self, block: &Block) -> Result<Self::Good, Self::Bad> {
        self.push_scope();
        for node in block.iter() {
            self.visit_node(node)?;
        }
        self.pop_scope();
        Ok(())
    }

    fn visit_literal(&mut self, _: &crate::ast::Literal) -> Result<Self::Good, Self::Bad> {
        Ok(())
    }

    fn visit_binary(
        &mut self,
        _: crate::ast::BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(lhs)?;
        self.visit_expr(rhs)
    }

    fn visit_unary(
        &mut self,
        _: crate::ast::UnaryOperator,
        e: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(e)
    }

    fn visit_variable(&mut self, var: &crate::ast::Identifier) -> Result<Self::Good, Self::Bad> {
        let name = &var.name;
        if self.in_local_scope() {
            if self
                .scopes
                .get_local_and_capture(name, self.current_scope, self.closure_boundary)
                .is_some()
            {
                return Ok(());
            }
            if self.scopes.lookup_global(name).is_err() {
                self.scopes
                    .found_unknown_variable(name.clone(), self.closure_boundary > 0)?
            }
        } else {
            self.scopes.lookup_global(name)?;
        }
        Ok(())
    }

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

    fn visit_while(&mut self, cond: &Expression, body: &Block) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(cond)?;
        self.visit_block(body)
    }

    fn visit_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Self::Good, Self::Bad> {
        for arg in args.iter() {
            self.visit_expr(arg)?;
        }
        self.visit_expr(func)
    }

    fn visit_lambda(
        &mut self,
        params: &[crate::ast::Identifier],
        body: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        self.push_scope();
        for param in params.iter() {
            // parameters are mutable by default
            self.scopes
                .make_declaration(self.current_scope, param.name.clone(), false)?;
        }
        let saved_closure_boundary = self.closure_boundary;
        self.closure_boundary = self.current_scope;
        self.visit_block(body)?;
        self.closure_boundary = saved_closure_boundary;
        self.pop_scope();
        Ok(())
    }

    fn visit_assignment(
        &mut self,
        id: &crate::ast::Lvalue,
        e: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        let name = id.name().unwrap();
        self.scopes
            .make_assignment(name, self.current_scope, self.closure_boundary)?;

        self.visit_expr(e)
    }

    fn visit_declaration(
        &mut self,
        id: &crate::ast::Identifier,
        e: &Expression,
        is_const: bool,
    ) -> Result<Self::Good, Self::Bad> {
        self.scopes
            .make_declaration(self.current_scope, id.name.clone(), is_const)?;
        self.visit_expr(e)
    }
}
