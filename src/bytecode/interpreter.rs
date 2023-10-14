use std::{
    cell::Cell,
    collections::{HashMap, HashSet},
};

use crate::{
    error::{Error, RuntimeError},
    interpreter::Interpreter,
    parser::Parser,
};

use super::{
    compiler::{BytecodeCompiler, Program},
    instruction::{Instruction, Source},
    value::Value,
};

#[derive(Default)]
pub struct BytecodeInterpreter {
    program: Vec<Instruction>,
    vm: VirtualMachine,
}

#[derive(Default)]
struct VirtualMachine {
    ip: usize,
    result: Value,
    stack_p: Cell<usize>,
    stack: Vec<Value>,
    local: Vec<Value>,
    globals: HashMap<String, Value>,
    global_consts: HashSet<String>,
}
impl VirtualMachine {
    fn push_stack_p(&self) -> usize {
        let index = self.stack_p.get();
        self.stack_p.set(index + 1);
        index
    }

    fn pop_stack_p(&self) -> usize {
        let index = self.stack_p.get() - 1;
        self.stack_p.set(index);
        index
    }

    fn fetch<'a>(&'a self, location: &'a Source) -> Result<&'a Value, Error> {
        Ok(match location {
            Source::Result => &self.result,
            Source::Immediate(imm) => imm,
            Source::Local(index) => {
                let index = self.local.len() - index - 1;
                self.local.get(index).expect(
                    "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
                )
            }
            Source::Stack => self.stack.get(self.pop_stack_p()).unwrap(),
            Source::Global(name) => match self.globals.get(name) {
                Some(value) => value,
                None => return Err(RuntimeError::UnknownIdentifier(name.clone()).into()),
            },
        })
    }

    fn store(&mut self, location: &Source) -> Result<(), Error> {
        match location {
            Source::Result => {}
            Source::Immediate(_) => panic!("Cannot store to immediate value"),
            Source::Local(index) => {
                let index = self.local.len() - index - 1;
                *self.local.get_mut(index).expect(
                    "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
                ) = self.result.clone()
            }
            Source::Stack => {
                let index = self.push_stack_p();
                if index == self.stack.len() {
                    self.stack.push(self.result.clone());
                } else {
                    self.stack[index] = self.result.clone();
                }
            }
            Source::Global(name) => match self.globals.get(name) {
                None => {
                    self.globals.insert(name.clone(), self.result.clone());
                }
                Some(_) => {
                    if self.global_consts.contains(name) {
                        return Err(RuntimeError::ConstReassignment(name.to_owned()).into());
                    } else {
                        self.globals.insert(name.clone(), self.result.clone());
                    }
                }
            },
        };
        Ok(())
    }

    fn alloc_locals(&mut self, num: usize) {
        for _ in 0..num {
            self.local.push(Default::default())
        }
    }

    fn dealloc_locals(&mut self, num: usize) {
        for _ in 0..num {
            self.local.pop();
        }
    }

    fn jmp(&mut self, dest: usize) {
        self.ip = dest;
    }
}

impl Interpreter for BytecodeInterpreter {
    type ReplReturn = Value;

    fn new() -> Self {
        Self::default()
    }

    fn interpret(
        &mut self,
        text: &str,
        filename: Option<std::rc::Rc<str>>,
    ) -> Vec<Result<Self::ReplReturn, Error>> {
        let mut ret: Vec<Result<_, Error>> = Default::default();
        let mut parser = {
            match Parser::new(text, filename) {
                Ok(parser) => parser,
                Err(e) => {
                    ret.push(Err(e));
                    return ret;
                }
            }
        };
        let gen = parser.gen_ast();
        let ast = match gen {
            Ok(ast) => ast,
            Err(e) => {
                ret.push(Err(e));
                return ret;
            }
        };
        let bytecode = match BytecodeCompiler::compile_bytecode(ast) {
            Ok(Program {
                instructions,
                global_consts,
            }) => {
                self.vm.global_consts.extend(global_consts);
                instructions
            }
            Err(e) => {
                ret.push(Err(e));
                return ret;
            }
        };
        ret.push(self.interpret_bytecode(bytecode));
        ret
    }
}

impl BytecodeInterpreter {
    pub fn interpret_bytecode(
        &mut self,
        program: Vec<Instruction>,
    ) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        self.program = program;
        self.vm.ip = 0;
        self.run_program()
    }

    fn run_program(&mut self) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        let vm = &mut self.vm;
        while vm.ip < self.program.len() {
            println!("ip: {}; {:?}", vm.ip, &self.program[vm.ip]);
            match &self.program[vm.ip] {
                Instruction::Nullary { src } => vm.result = vm.fetch(src)?.clone(),
                Instruction::Binary { op, src0, src1 } => {
                    vm.result = Value::binary_operation(*op, vm.fetch(src0)?, vm.fetch(src1)?)?;
                }
                Instruction::Unary { op, src0 } => {
                    vm.result = Value::unary_operation(*op, vm.fetch(src0)?)?;
                }
                Instruction::CreateScope { locals } => vm.alloc_locals(*locals),
                Instruction::DestroyScope { locals } => vm.dealloc_locals(*locals),
                Instruction::JumpTrue { jump_dest } => {
                    if vm.result.boolean()? {
                        vm.jmp(*jump_dest);
                        continue;
                    }
                }
                Instruction::JumpFalse { jump_dest } => {
                    if !vm.result.boolean()? {
                        vm.jmp(*jump_dest);
                        continue;
                    }
                }
                Instruction::UnconditionalJump { jump_dest } => {
                    vm.jmp(*jump_dest);
                    continue;
                }
                Instruction::Store { dest } => vm.store(dest)?,
                Instruction::Noop => {}
            }
            vm.ip += 1;
        }
        Ok(vm.result.clone())
    }
}
