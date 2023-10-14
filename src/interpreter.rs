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
