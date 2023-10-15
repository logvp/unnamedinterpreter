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
    chunks: Vec<Vec<Instruction>>,
    call_stack: Vec<(usize, usize)>,
    vm: VirtualMachine,
}

#[derive(Default)]
struct VirtualMachine {
    ip: usize,
    result: Value,
    stack_p: Cell<usize>,
    stack: Vec<Value>,
    local: Vec<Value>,
    arguments: Vec<Value>,
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
            Source::Arguments => unimplemented!(),
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
            Source::Arguments => self.arguments.push(self.result.clone()),
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

    fn load_arguments(&mut self) {
        while self.arguments.len() != 0 {
            let index = self.local.len() - self.arguments.len();
            *self.local.get_mut(index).expect(
                "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
            ) = self.arguments.pop().unwrap()
        }
    }

    fn alloc_locals(&mut self, num: usize) {
        self.result = Value::None;
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
        let program = match BytecodeCompiler::compile_bytecode(ast) {
            Ok(program) => program,
            Err(e) => {
                ret.push(Err(e));
                return ret;
            }
        };
        ret.push(self.run_program(program));
        ret
    }
}

impl BytecodeInterpreter {
    pub fn run_program(
        &mut self,
        program: Program,
    ) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        let Program {
            instructions,
            global_consts,
        } = program;
        self.vm.global_consts.extend(global_consts);
        self.chunks = instructions;
        self.call_stack.push((0, 0));
        self.vm.ip = 0;
        self.run()
    }

    fn run(&mut self) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        // for routine in self.chunks.iter() {
        //     println!("Routine:");
        //     for instr in routine.iter() {
        //         println!("{:?}", instr);
        //     }
        //     println!();
        // }
        // return Ok(Value::None);

        let vm = &mut self.vm;
        while let Some((_, routine_index)) = self.call_stack.last() {
            let program = &self.chunks[*routine_index];
            if vm.ip >= program.len() {
                if let Some((ip, _)) = self.call_stack.pop() {
                    vm.ip = ip;
                    continue;
                } else {
                    break;
                }
            }
            // println!(
            //     "routine: {}; ip: {}; {:?}",
            //     routine_index, vm.ip, program[vm.ip]
            // );
            match &program[vm.ip] {
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
                Instruction::Call => {
                    let f = vm.result.function()?;
                    if f.arity != vm.arguments.len() {
                        return Err(RuntimeError::ExpectedArgumentsFound(
                            f.arity,
                            vm.arguments.len(),
                        )
                        .into());
                    }
                    self.call_stack.push((vm.ip + 1, f.code));
                    vm.ip = 0;
                    continue;
                }
                Instruction::CallIntrinsic => todo!(),
                Instruction::LoadArguments => vm.load_arguments(),
                Instruction::Noop => {}
            }
            vm.ip += 1;
        }
        Ok(vm.result.clone())
    }
}
