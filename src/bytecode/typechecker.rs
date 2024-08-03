use crate::{ast::Ast, error::Error};

use super::resolver::ResolutionTable;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn check(_ast: &Ast, _variables: &ResolutionTable) -> Result<(), Error> {
        println!("Type checking not implemented");
        Ok(())
    }
}
