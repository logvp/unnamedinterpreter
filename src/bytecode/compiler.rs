use std::rc::Rc;

use crate::{
    ast::{Ast, AstNode, Block, Expression, Statement},
    error::Error,
};

use super::{
    instruction::{Instruction, Source},
    resolver::{LocalVariable, ResolutionTable},
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
    ) -> Result<ProgramChunk, Error> {
        let mut compiler = BytecodeCompiler::new(start_index, variables);
        for node in &ast.nodes {
            compiler.compile_node(node)?;
        }
        Ok(ProgramChunk {
            starts_at: compiler.start_index,
            procedures: compiler.procedures,
        })
    }

    fn resolve(&self, name: Rc<str>) -> Source {
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

    fn compile_node(&mut self, node: &AstNode) -> Result<(), Error> {
        match node {
            AstNode::Expression(expr) => self.compile_expr(expr),
            AstNode::Statement(stmt) => self.compile_stmt(stmt),
        }
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> Result<(), Error> {
        match stmt {
            Statement::Assignment(lvalue, expr) => {
                // if local was declared const, return error
                let dest = self.resolve(lvalue.name().unwrap());
                self.compile_expr(expr)?;
                self.push_instruction(Instruction::Store { dest });
            }
            Statement::Declaration(ident, expr, _) => {
                let dest = self.resolve(Rc::clone(&ident.name));
                self.compile_expr(expr)?;
                self.push_instruction(Instruction::Store { dest });
            }
            Statement::Expression(expr) => self.compile_expr(expr)?,
        }
        self.push_instruction(Instruction::Nullary {
            src: Source::Immediate(Value::None),
        });
        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expression) -> Result<(), Error> {
        match expr {
            Expression::Binary(op, lhs, rhs) => {
                let mut src0 = match lhs.as_ref() {
                    Expression::Literal(literal) => Source::Immediate(Value::from(literal)),
                    Expression::Variable(ident) => self.resolve(Rc::clone(&ident.name)),
                    expr => {
                        self.compile_expr(expr)?;
                        Source::Result
                    }
                };
                let src1 = match rhs.as_ref() {
                    Expression::Literal(literal) => Source::Immediate(Value::from(literal)),
                    Expression::Variable(ident) => self.resolve(Rc::clone(&ident.name)),
                    expr => {
                        // save lhs
                        if !matches!(src0, Source::Result) {
                            self.push_instruction(Instruction::Nullary { src: src0 });
                        }
                        self.push_instruction(Instruction::Store {
                            dest: Source::Stack,
                        });
                        src0 = Source::Stack;
                        self.compile_expr(expr)?;
                        Source::Result
                    }
                };
                self.push_instruction(Instruction::Binary {
                    op: *op,
                    src0,
                    src1,
                });
                Ok(())
            }
            Expression::Unary(op, lhs) => {
                self.compile_expr(lhs)?;
                self.push_instruction(Instruction::Unary {
                    op: *op,
                    src0: Source::Result,
                });
                Ok(())
            }
            Expression::Literal(literal) => {
                self.push_instruction(Instruction::Nullary {
                    src: Source::Immediate(Value::from(literal)),
                });
                Ok(())
            }
            Expression::Variable(ident) => {
                let src = self.resolve(Rc::clone(&ident.name));
                self.push_instruction(Instruction::Nullary { src });
                Ok(())
            }
            Expression::Block(block) => self.compile_block(block),
            Expression::IfElse(expr, if_block, else_block) => {
                self.compile_expr(expr)?;
                let begin_if_index = self.instruction_index();
                self.push_instruction(Instruction::Noop);
                self.compile_block(if_block)?;
                let end_if_index = self.instruction_index();
                self.push_instruction(Instruction::Noop);
                self.compile_block(else_block)?;
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
            Expression::While(expr, body) => {
                let continue_index = self.instruction_index();
                self.compile_expr(expr)?;
                let condition_jump_index = self.instruction_index();
                self.push_instruction(Instruction::Noop);
                self.compile_block(body)?;
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
            Expression::FunctionCall(fun, arguments) => {
                for arg in arguments.iter() {
                    // evaluate arguments in order and push them onto the argument stack
                    self.compile_expr(arg)?;
                    self.push_instruction(Instruction::Store {
                        dest: Source::Stack,
                    });
                }

                // call instruction with the function object in result
                self.compile_expr(fun)?;
                self.push_instruction(Instruction::Call {
                    argc: arguments.len(),
                });
                Ok(())
            }
            Expression::Lambda(parameters, body) => {
                // Compile the function body in a new procedure
                let procedure_id = self.push_procedure();
                self.push_instruction(Instruction::CreateScope {
                    locals: parameters.len(),
                });
                for param in parameters.iter() {
                    self.resolve(Rc::clone(&param.name));
                }
                for param in parameters.iter().rev() {
                    self.push_instruction(Instruction::Nullary { src: Source::Stack });
                    self.push_instruction(Instruction::Store {
                        dest: self.resolve(Rc::clone(&param.name)),
                    })
                }
                self.compile_block(body)?;
                self.push_instruction(Instruction::DestroyScope {
                    locals: parameters.len(),
                });
                self.pop_procedure();
                // end the function compilation, now push the function object to the Result
                self.push_instruction(Instruction::FunctionLiteral {
                    arity: parameters.len(),
                    procedure_id,
                });
                Ok(())
            }
            x => todo!("Compiling {:?} is not implemented yet", x),
        }
    }

    fn compile_block(&mut self, block: &Block) -> Result<(), Error> {
        let Block(nodes) = block;
        // new block = new scope
        self.push_scope();
        let start_index = self.instruction_index();
        self.push_instruction(Instruction::Noop); // Placeholder for CreateScope because number of locals is unknown
        for node in nodes.iter() {
            self.compile_node(node)?;
        }
        let locals = self.variables.get_num_locals_in_scope(self.current_scope);
        self.patch_instruction(start_index, Instruction::CreateScope { locals });
        self.push_instruction(Instruction::DestroyScope { locals }); // could be omitted if locals == 0
        self.pop_scope(); // assert scope was still on stack
        Ok(())
    }
}
