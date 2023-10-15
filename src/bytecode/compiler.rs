use std::collections::{HashMap, HashSet};

use crate::{
    ast::{Ast, AstNode, Block, Expression, Statement},
    error::{Error, RuntimeError},
};

use super::{
    instruction::{Instruction, Source},
    value::Value,
};

pub struct Program {
    pub instructions: Vec<Instruction>,
    pub global_consts: HashSet<String>,
}

#[derive(Default)]
pub struct BytecodeCompiler {
    program: Vec<Instruction>,
    scopes: Vec<HashMap<String, (bool, usize)>>,
    global_consts: HashSet<String>,
}
impl BytecodeCompiler {
    pub fn compile_bytecode(ast: Ast) -> Result<Program, Error> {
        let mut compiler = Self::default();
        for node in ast.nodes.iter() {
            compiler.compile_node(node)?;
        }
        Ok(Program {
            instructions: compiler.program,
            global_consts: compiler.global_consts,
        })
    }

    fn resolve(&self, name: &str) -> (bool, Source) {
        let mut parent_depth = 0;
        for scope in self.scopes.iter().rev() {
            if let Some((is_const, index)) = scope.get(name) {
                return (*is_const, Source::Local(index + parent_depth));
            }
            parent_depth += scope.len();
        }
        (false, Source::Global(name.to_string()))
    }

    fn declare(&mut self, name: &str, is_const: bool) -> Source {
        if let Some(scope) = self.scopes.last_mut() {
            let index = scope.len();
            let None = scope.insert(name.to_owned(), (is_const, index)) else {
                todo!("handle redeclaration")
            };
            Source::Local(index)
        } else {
            if is_const {
                self.global_consts.insert(name.to_owned());
            }
            Source::Global(name.to_owned())
        }
    }

    fn push_instruction(&mut self, instr: Instruction) {
        self.program.push(instr)
    }

    fn patch_instruction(&mut self, index: usize, instr: Instruction) {
        self.program[index] = instr;
    }

    fn instruction_index(&self) -> usize {
        self.program.len()
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
                let (false, dest) = self.resolve(lvalue.name().unwrap()) else {
                    return Err(RuntimeError::ConstReassignment(lvalue.name().unwrap().to_owned()).into())
                };
                self.compile_expr(expr)?;
                self.push_instruction(Instruction::Store { dest });
            }
            Statement::Declaration(ident, expr, is_const) => {
                let dest = self.declare(&ident.name, *is_const);
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
                    Expression::Literal(literal) => Source::Immediate(Value::from(literal.clone())),
                    Expression::Variable(ident) => self.resolve(&ident.name).1,
                    expr => {
                        self.compile_expr(expr)?;
                        Source::Result
                    }
                };
                let src1 = match rhs.as_ref() {
                    Expression::Literal(literal) => Source::Immediate(Value::from(literal.clone())),
                    Expression::Variable(ident) => self.resolve(&ident.name).1,
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
                    src: Source::Immediate(Value::from(literal.clone())),
                });
                Ok(())
            }
            Expression::Variable(ident) => {
                let (_, src) = self.resolve(&ident.name);
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
            x => todo!("Compiling {:?} is not implemented yet", x),
        }
    }

    fn compile_block(&mut self, block: &Block) -> Result<(), Error> {
        let Block(nodes) = block;
        // new block = new scope
        self.scopes.push(Default::default());
        let start_index = self.instruction_index();
        self.push_instruction(Instruction::Noop); // Placeholder for CreateScope because number of locals is unknown
        for node in nodes.iter() {
            self.compile_node(node)?;
        }
        let locals = self.scopes.last().unwrap().len();
        self.patch_instruction(start_index, Instruction::CreateScope { locals });
        self.push_instruction(Instruction::DestroyScope { locals }); // could be omitted if locals == 0
        self.scopes.pop().unwrap(); // assert scope was still on stack
        Ok(())
    }
}
