use crate::{
    argp::Options,
    ast::{Ast, Block, Expression, Statement},
    error::Error,
    visitor::AstVisitor,
    Symbol,
};

use super::{
    instruction::{Instruction, Source},
    resolver::{LocalVariable, ResolutionTable},
    typechecker::TypeChecker,
    value::Value,
};

pub struct ProgramChunk {
    pub starts_at: usize,
    pub procedures: Vec<Vec<Instruction>>,
}

pub(super) struct BytecodeCompiler<'a> {
    start_index: usize,
    procedures: Vec<Vec<Instruction>>,
    procedure_index: Vec<usize>,
    variables: &'a ResolutionTable,
    current_scope: usize,
    next_scope: usize,
}
impl<'a> BytecodeCompiler<'a> {
    fn new(start_index: usize, variables: &'a ResolutionTable) -> Self {
        BytecodeCompiler {
            start_index,
            procedures: vec![Default::default()],
            procedure_index: vec![0],
            variables,
            current_scope: 0,
            next_scope: 1,
        }
    }

    pub fn gen_bytecode(
        ast: Ast,
        start_index: usize,
        variables: &ResolutionTable,
        options: &Options,
    ) -> Result<ProgramChunk, Error> {
        TypeChecker::check(&ast, options.unify_branches)?;
        let mut compiler = BytecodeCompiler::new(start_index, variables);
        compiler.visit_ast(&ast)?;
        Ok(ProgramChunk {
            starts_at: compiler.start_index,
            procedures: compiler.procedures,
        })
    }

    fn resolve(&self, name: Symbol) -> Source {
        let mut parent_depth = 0;
        let mut scope = self.current_scope;
        while scope > 0 {
            match self.variables.lookup_local_in_scope(&name, scope) {
                Some(LocalVariable::Local { index, .. }) => {
                    return Source::Local(index.unwrap() + parent_depth);
                }
                Some(LocalVariable::Captured { .. }) => return Source::Env(name),
                None => parent_depth += self.variables.get_num_locals_in_scope(scope),
            }
            scope = self.variables.parent_of(scope);
        }
        Source::Global(name)
    }

    fn push_scope(&mut self) {
        self.current_scope = self.next_scope;
        self.next_scope += 1;
    }

    fn pop_scope(&mut self) {
        self.current_scope = self.variables.parent_of(self.current_scope);
    }

    fn push_procedure(&mut self) -> usize {
        self.push_scope();
        let index = self.procedures.len();
        self.procedures.push(Default::default());
        self.procedure_index.push(index);
        index + self.start_index
    }

    fn pop_procedure(&mut self) {
        self.pop_scope();
        self.procedure_index
            .pop()
            .expect("Procedure stack should never be empty");
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.procedures[*self.procedure_index.last().unwrap()].push(instr)
    }

    fn patch_instruction(&mut self, index: usize, instr: Instruction) {
        self.procedures[*self.procedure_index.last().unwrap()][index] = instr;
    }

    fn instruction_index(&self) -> usize {
        self.procedures[*self.procedure_index.last().unwrap()].len()
    }
}

impl<'a> AstVisitor for BytecodeCompiler<'a> {
    type Good = ();
    type Bad = Error;

    fn visit_statement(&mut self, stmt: &Statement) -> Result<Self::Good, Self::Bad> {
        match stmt {
            Statement::Assignment(lvalue, expr) => self.visit_assignment(lvalue, expr)?,
            Statement::Declaration(ident, expr, is_const) => {
                self.visit_declaration(ident, expr, *is_const)?
            }
            Statement::Expression(expr) => self.visit_expr(expr)?,
        }
        self.push_instruction(Instruction::Nullary {
            src: Source::Immediate(Value::None),
        });
        Ok(())
    }

    fn visit_binary(
        &mut self,
        op: crate::ast::BinaryOperator,
        lhs: &Expression,
        rhs: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        let mut src0 = match lhs {
            Expression::Literal(literal) => Source::Immediate(Value::from(literal)),
            Expression::Variable(ident) => self.resolve(ident.name.clone()),
            expr => {
                self.visit_expr(expr)?;
                Source::Result
            }
        };
        let src1 = match rhs {
            Expression::Literal(literal) => Source::Immediate(Value::from(literal)),
            Expression::Variable(ident) => self.resolve(ident.name.clone()),
            expr => {
                // save lhs
                if !matches!(src0, Source::Result) {
                    self.push_instruction(Instruction::Nullary { src: src0 });
                }
                self.push_instruction(Instruction::Store {
                    dest: Source::Stack,
                });
                src0 = Source::Stack;
                self.visit_expr(expr)?;
                Source::Result
            }
        };
        self.push_instruction(Instruction::Binary { op, src0, src1 });
        Ok(())
    }

    fn visit_unary(
        &mut self,
        op: crate::ast::UnaryOperator,
        e: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(e)?;
        self.push_instruction(Instruction::Unary {
            op,
            src0: Source::Result,
        });
        Ok(())
    }

    fn visit_literal(&mut self, literal: &crate::ast::Literal) -> Result<Self::Good, Self::Bad> {
        self.push_instruction(Instruction::Nullary {
            src: Source::Immediate(Value::from(literal)),
        });
        Ok(())
    }

    fn visit_variable(&mut self, ident: &crate::ast::Identifier) -> Result<Self::Good, Self::Bad> {
        let src = self.resolve(ident.name.clone());
        self.push_instruction(Instruction::Nullary { src });
        Ok(())
    }

    fn visit_if_else(
        &mut self,
        cond: &Expression,
        if_true: &Block,
        if_false: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        self.visit_expr(cond)?;
        let begin_if_index = self.instruction_index();
        self.push_instruction(Instruction::Noop);
        self.visit_block(if_true)?;
        let end_if_index = self.instruction_index();
        self.push_instruction(Instruction::Noop);
        self.visit_block(if_false)?;
        let end_else_index = self.instruction_index();

        self.patch_instruction(
            begin_if_index,
            Instruction::JumpFalse {
                jump_dest: end_if_index + 1,
            },
        );
        self.patch_instruction(
            end_if_index,
            Instruction::UnconditionalJump {
                jump_dest: end_else_index,
            },
        );
        Ok(())
    }

    fn visit_while(&mut self, cond: &Expression, body: &Block) -> Result<Self::Good, Self::Bad> {
        let continue_index = self.instruction_index();
        self.visit_expr(cond)?;
        let condition_jump_index = self.instruction_index();
        self.push_instruction(Instruction::Noop);
        self.visit_block(body)?;
        self.push_instruction(Instruction::UnconditionalJump {
            jump_dest: continue_index,
        });
        let break_index = self.instruction_index();

        self.patch_instruction(
            condition_jump_index,
            Instruction::JumpFalse {
                jump_dest: break_index,
            },
        );
        Ok(())
    }

    fn visit_function_call(
        &mut self,
        func: &Expression,
        args: &[Expression],
    ) -> Result<Self::Good, Self::Bad> {
        for arg in args.iter() {
            // evaluate arguments in order and push them onto the argument stack
            self.visit_expr(arg)?;
            self.push_instruction(Instruction::Store {
                dest: Source::Stack,
            });
        }

        // call instruction with the function object in result
        self.visit_expr(func)?;
        self.push_instruction(Instruction::Call { argc: args.len() });
        Ok(())
    }

    fn visit_lambda(
        &mut self,
        params: &[crate::ast::Identifier],
        body: &Block,
    ) -> Result<Self::Good, Self::Bad> {
        // Compile the function body in a new procedure
        let procedure_id = self.push_procedure();
        self.push_instruction(Instruction::CreateScope {
            locals: params.len(),
        });
        for param in params.iter() {
            self.resolve(param.name.clone());
        }
        for param in params.iter().rev() {
            self.push_instruction(Instruction::Nullary { src: Source::Stack });
            self.push_instruction(Instruction::Store {
                dest: self.resolve(param.name.clone()),
            })
        }
        self.visit_block(body)?;
        self.push_instruction(Instruction::DestroyScope {
            locals: params.len(),
        });
        self.pop_procedure();
        // end the function compilation, now push the function object to the Result
        self.push_instruction(Instruction::FunctionLiteral {
            arity: params.len(),
            procedure_id,
        });
        Ok(())
    }

    fn visit_block(&mut self, block: &Block) -> Result<Self::Good, Self::Bad> {
        // new block = new scope
        self.push_scope();
        let start_index = self.instruction_index();
        self.push_instruction(Instruction::Noop); // Placeholder for CreateScope because number of locals is unknown
        for node in block.iter() {
            self.visit_node(node)?;
        }
        let locals = self.variables.get_num_locals_in_scope(self.current_scope);
        self.patch_instruction(start_index, Instruction::CreateScope { locals });
        self.push_instruction(Instruction::DestroyScope { locals }); // could be omitted if locals == 0
        self.pop_scope(); // assert scope was still on stack
        Ok(())
    }

    fn visit_assignment(
        &mut self,
        lvalue: &crate::ast::Lvalue,
        expr: &Expression,
    ) -> Result<Self::Good, Self::Bad> {
        // if local was declared const, return error
        let dest = self.resolve(lvalue.name().unwrap());
        self.visit_expr(expr)?;
        self.push_instruction(Instruction::Store { dest });
        Ok(())
    }

    fn visit_declaration(
        &mut self,
        ident: &crate::ast::Identifier,
        expr: &Expression,
        _const: bool,
    ) -> Result<Self::Good, Self::Bad> {
        let dest = self.resolve(ident.name.clone());
        self.visit_expr(expr)?;
        self.push_instruction(Instruction::Store { dest });
        Ok(())
    }
}
