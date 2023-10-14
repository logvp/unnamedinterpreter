use std::{fmt::Display, rc::Rc};

use crate::error::Error;

pub trait Interpreter {
    type ReplReturn: Display;

    fn new() -> Self;

    fn interpret(
        &mut self,
        text: &str,
        filename: Option<Rc<str>>,
    ) -> Vec<Result<Self::ReplReturn, Error>>;
}

#[derive(Debug)]
pub enum RuntimeType {
    Object,
    Function,
    Integer,
    String,
    Boolean,
    None,
}
impl Display for RuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Object => "Object",
                Self::Function => "Function",
                Self::Integer => "Integer",
                Self::String => "String",
                Self::Boolean => "Boolean",
                Self::None => "NoneType",
            }
        )
    }
}
