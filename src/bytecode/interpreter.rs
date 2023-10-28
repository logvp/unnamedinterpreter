use std::{cell::Cell, collections::HashMap, rc::Rc};

use crate::{
    error::{Error, RuntimeError},
    interpreter::Interpreter,
    parser::Parser,
};

use super::{
    compiler::{BytecodeCompiler, ProgramChunk},
    instruction::{Instruction, Source},
    intrinsics,
    resolver::{GlobalVariable, ResolutionTable, Resolver},
    value::{FunctionObject, Value},
};

type CallStack = Vec<(usize, usize)>;
pub struct BytecodeInterpreter {
    procedures: Vec<Vec<Instruction>>,
    call_stack: CallStack,
    vm: VirtualMachine,
    resolver: Resolver,
}

#[derive(Default)]
pub(super) struct VirtualMachine {
    pub ip: usize,
    pub result: Cell<Value>,
    pub stack_p: Cell<usize>,
    pub stack: Vec<Value>,
    pub local: Vec<Value>,
    pub globals: HashMap<Rc<str>, Value>,
}
impl VirtualMachine {
    pub(super) fn push_stack_p(&self) -> usize {
        let index = self.stack_p.get();
        self.stack_p.set(index + 1);
        index
    }

    pub(super) fn pop_stack_p(&self) -> usize {
        let index = self.stack_p.get() - 1;
        self.stack_p.set(index);
        index
    }

    pub(super) fn pop_many_stack_p(&self, n: usize) -> usize {
        let index = self.stack_p.get() - n;
        self.stack_p.set(index);
        index
    }

    fn fetch(&self, location: &Source) -> Result<Value, Error> {
        Ok(match location {
            Source::Result => self.result.take(),

            Source::Immediate(imm) => imm.clone(),
            Source::Local(index) => {
                let index = self.local.len() - index - 1;
                self.local.get(index).expect(
                    "Attempt to read from unallocated local memory. Reserve memory with CreateScope",
                ).clone()
            }
            Source::Stack => self.stack.get(self.pop_stack_p()).unwrap().clone(),
            Source::Global(name) => match self.globals.get(name) {
                Some(value) => value.clone(),
                None => return Err(RuntimeError::UnknownIdentifier(name.to_string()).into()),
            },
        })
    }

    fn store(&mut self, location: &Source, variables: &ResolutionTable) -> Result<(), Error> {
        match location {
            Source::Result => {}
            Source::Immediate(_) => panic!("Cannot store to immediate value"),
            Source::Local(index) => {
                let index = self.local.len() - index - 1;
                *self.local.get_mut(index).expect(
                    "Attempt to store to unallocated local memory. Reserve memory with CreateScope",
                ) = self.result.take();
            }
            Source::Stack => {
                let index = self.push_stack_p();
                if index == self.stack.len() {
                    self.stack.push(self.result.take());
                } else {
                    self.stack[index] = self.result.take();
                }
            }
            Source::Global(name) => match variables.lookup_global(name)? {
                GlobalVariable::Constant => {
                    if self.globals.contains_key(name.as_ref()) {
                        return Err(RuntimeError::ConstReassignment(name.to_string()).into());
                    } else {
                        self.globals.insert(Rc::clone(name), self.result.take());
                    }
                }
                GlobalVariable::NotConstant => {
                    self.globals.insert(Rc::clone(name), self.result.take());
                }
                GlobalVariable::Unknown => unreachable!(),
            },
        };
        Ok(())
    }

    fn alloc_locals(&mut self, num: usize) {
        let _ = self.result.take();
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
        let mut interpreter = BytecodeInterpreter {
            vm: Default::default(),
            resolver: Resolver::new(),
            call_stack: Default::default(),
            procedures: Default::default(),
        };
        interpreter.define_intrinsics();
        interpreter
    }

    fn interpret(
        &mut self,
        text: &str,
        filename: Option<std::rc::Rc<str>>,
    ) -> Vec<Result<Self::ReplReturn, Error>> {
        let ast = match Parser::gen_ast(text, filename) {
            Ok(ast) => ast,
            Err(e) => return vec![Err(e)],
        };
        if let Err(e) = self.resolver.resolve(&ast) {
            return vec![Err(e)];
        }
        let program = match BytecodeCompiler::gen_bytecode(
            ast,
            self.procedures.len(),
            self.resolver.get_table(),
        ) {
            Ok(program) => program,
            Err(e) => return vec![Err(e)],
        };
        vec![self.run_program(program)]
    }
}

impl BytecodeInterpreter {
    fn define_intrinsics(&mut self) {
        self.vm
            .globals
            .extend(intrinsics::INTRINSICS.map(|intrinsic| {
                (
                    Rc::from(intrinsics::get_name(intrinsic)),
                    Value::Function(FunctionObject::Intrinsic(intrinsic)),
                )
            }));
        self.resolver.define_globals(
            &intrinsics::INTRINSICS
                .map(intrinsics::get_name)
                .map(Rc::from),
        );
    }

    pub fn run_program(
        &mut self,
        program: ProgramChunk,
    ) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        let ProgramChunk {
            starts_at,
            procedures,
        } = program;
        self.procedures.extend(procedures);
        self.call_stack.push((0, starts_at));
        self.vm.ip = 0;
        self.run()
    }

    fn run(&mut self) -> Result<<Self as Interpreter>::ReplReturn, Error> {
        // for routine in self.procedures.iter() {
        //     println!("Routine:");
        //     for instr in routine.iter() {
        //         println!("{:?}", instr);
        //     }
        //     println!();
        // }
        // return Ok(Value::None);

        let vm = &mut self.vm;
        while let Some((_, procedure_index)) = self.call_stack.last() {
            let program = &self.procedures[*procedure_index];
            if vm.ip >= program.len() {
                if let Some((ip, _)) = self.call_stack.pop() {
                    vm.ip = ip;
                    continue;
                } else {
                    break;
                }
            }
            // println!("{:?}", vm.local);
            // println!(
            //     "procedure: {}; ip: {}; {:?}",
            //     procedure_index, vm.ip, program[vm.ip]
            // );
            match BytecodeInterpreter::eval_instruction(
                &program[vm.ip],
                vm,
                self.resolver.get_table(),
                &mut self.call_stack,
            ) {
                Ok(true) => continue,
                Ok(false) => vm.ip += 1,
                Err(e) => {
                    // reset to a known safe state then return error
                    self.call_stack.clear();
                    return Err(e);
                }
            }
        }
        Ok(vm.result.take())
    }

    fn eval_instruction(
        instruction: &Instruction,
        vm: &mut VirtualMachine,
        variables: &ResolutionTable,
        call_stack: &mut CallStack,
    ) -> Result<bool, Error> {
        match instruction {
            Instruction::Nullary { src } => vm.result.set(vm.fetch(src)?),
            Instruction::Binary { op, src0, src1 } => {
                vm.result.set(Value::binary_operation(
                    *op,
                    vm.fetch(src0)?,
                    vm.fetch(src1)?,
                )?);
            }
            Instruction::Unary { op, src0 } => {
                vm.result.set(Value::unary_operation(*op, vm.fetch(src0)?)?);
            }
            Instruction::CreateScope { locals } => vm.alloc_locals(*locals),
            Instruction::DestroyScope { locals } => vm.dealloc_locals(*locals),
            Instruction::JumpTrue { jump_dest } => {
                if vm.result.take().boolean()? {
                    vm.jmp(*jump_dest);
                    return Ok(true);
                }
            }
            Instruction::JumpFalse { jump_dest } => {
                if !vm.result.take().boolean()? {
                    vm.jmp(*jump_dest);
                    return Ok(true);
                }
            }
            Instruction::UnconditionalJump { jump_dest } => {
                vm.jmp(*jump_dest);
                return Ok(true);
            }
            Instruction::Store { dest } => vm.store(dest, variables)?,
            Instruction::Call { argc } => match vm.result.take().function()? {
                FunctionObject::Lambda {
                    arity,
                    procedure_id: code,
                } => {
                    if arity != *argc {
                        return Err(RuntimeError::ExpectedArgumentsFound(arity, *argc).into());
                    }
                    call_stack.push((vm.ip + 1, code));
                    vm.ip = 0;
                    return Ok(true);
                }
                FunctionObject::Intrinsic(intrinsic) => {
                    intrinsic.exec(*argc, vm)?;
                }
            },
            Instruction::Noop => {}
        }
        Ok(false)
    }
}
