use std::rc::Rc;

use crate::{
    bytecode::value::Value,
    error::{Error, RuntimeError},
};

use super::interpreter::VirtualMachine;

#[derive(Debug, Clone)]
pub enum IntrinsicFunction {
    Print,
    TypeOf,
    Debug,
}

impl IntrinsicFunction {
    pub(super) fn exec(&self, argc: usize, vm: &mut VirtualMachine) -> Result<(), Error> {
        match self {
            IntrinsicFunction::Print => {
                for arg in vm.stack.drain(vm.stack.len() - argc..) {
                    print!("{} ", arg)
                }
                println!();
                vm.pop_many_stack_p(argc);
                Ok(())
            }
            IntrinsicFunction::TypeOf => {
                if argc != 1 {
                    Err(RuntimeError::ExpectedArgumentsFound(1, argc).into())
                } else {
                    vm.result.set(Value::String(
                        Rc::from(format!("{}", vm.stack.last().unwrap().type_of())), // TODO: STRING_ALLOCATION
                    ));
                    vm.pop_stack_p();
                    Ok(())
                }
            }
            IntrinsicFunction::Debug => {
                for arg in vm.stack.drain(vm.stack.len() - argc..) {
                    print!("{:?} ", arg)
                }
                println!();
                vm.pop_many_stack_p(argc);
                Ok(())
            }
        }
    }
}
