use std::collections::HashMap;

use crate::{error::Error, interpreter::Interpreter};

use super::{
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
    temporary: Vec<Value>,
    local: Vec<Value>,
    global: HashMap<String, Value>,
}
impl VirtualMachine {
    fn fetch<'a>(&'a self, location: &'a Source) -> &'a Value {
        match location {
            Source::Result => &self.result,
            Source::Immediate(imm) => imm,
            Source::Local(index) => self.local.get(*index).expect(
                "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
            ),
            x => todo!("Fetching from {:?} is not implemented", x),
        }
    }

    fn store(&mut self, location: &Source) {
        match location {
            Source::Result => {}
            Source::Immediate(_) => panic!("Cannot store to immediate value"),
            Source::Local(index) => {
                *self.local.get_mut(*index).expect(
                    "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
                ) = self.result.clone()
            }
            x => todo!("Storing to {:?} is not implemented", x),
        }
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
        _text: &str,
        _filename: Option<std::rc::Rc<str>>,
    ) -> Vec<Result<Self::ReplReturn, Error>> {
        todo!()
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
            match &self.program[vm.ip] {
                Instruction::Binary { op, src0, src1 } => {
                    vm.result = Value::binary_operation(*op, vm.fetch(src0), vm.fetch(src1))?;
                }
                Instruction::Unary { op, src0 } => {
                    vm.result = Value::unary_operation(*op, vm.fetch(src0))?;
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
                Instruction::Noop => {}
                Instruction::Store { dest } => vm.store(dest),
            }
            vm.ip += 1;
        }
        Ok(vm.result.clone())
    }
}
