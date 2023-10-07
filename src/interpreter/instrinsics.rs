use crate::error::{Error, RuntimeError};

use super::interpreter::RuntimeValue;

#[derive(Debug)]
pub enum IntrinsicFunction {
    Print,
    TypeOf,
    Debug,
}

impl IntrinsicFunction {
    pub(super) fn call(&self, args: Vec<RuntimeValue>) -> Result<RuntimeValue, Error> {
        match self {
            IntrinsicFunction::Print => {
                for arg in args {
                    print!("{} ", arg)
                }
                println!();
                Ok(RuntimeValue::None)
            }
            IntrinsicFunction::TypeOf => {
                if args.len() != 1 {
                    Err(RuntimeError::ExpectedArgumentsFound(1, args.len()).into())
                } else {
                    Ok(RuntimeValue::String(format!("{}", args[0].get_type())))
                }
            }
            IntrinsicFunction::Debug => {
                for arg in args {
                    print!("{:?} ", arg)
                }
                println!();
                Ok(RuntimeValue::None)
            }
        }
    }
}
