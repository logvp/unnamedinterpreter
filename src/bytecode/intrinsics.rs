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
    pub(super) fn exec(&self, vm: &mut VirtualMachine) -> Result<(), Error> {
        match self {
            IntrinsicFunction::Print => {
                for arg in vm.arguments.iter() {
                    print!("{} ", arg)
                }
                println!();
                vm.arguments.clear();
                vm.result = Value::None;
                Ok(())
            }
            IntrinsicFunction::TypeOf => {
                if vm.arguments.len() != 1 {
                    Err(RuntimeError::ExpectedArgumentsFound(1, vm.arguments.len()).into())
                } else {
                    vm.result = Value::String(format!("{}", vm.arguments[0].type_of()));
                    Ok(())
                }
            }
            IntrinsicFunction::Debug => {
                for arg in vm.arguments.iter() {
                    print!("{:?} ", arg)
                }
                println!();
                Ok(())
            }
        }
    }
}
