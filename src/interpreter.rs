use std::collections::HashMap;

use crate::ast::*;
use crate::error::{Error, InterpreterError};
use crate::parser::Parser;

#[derive(Debug)]
pub enum Value {
    Int(i32),
    String(String),
}

impl Value {
    fn int(&self) -> Result<i32, InterpreterError> {
        if let Self::Int(int) = self {
            Ok(*int)
        } else {
            Err(InterpreterError::Error("Expected integer type".to_owned()))
        }
    }
}

pub struct Interpreter {
    context: Context,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Default::default(),
        }
    }
    fn interpret_node(&mut self, node: &AstNode) -> InterpreterReturn {
        node.eval(&self.context)
    }
    pub fn interpret(&mut self, text: String) -> Vec<InterpreterReturn> {
        let mut ret: Vec<InterpreterReturn> = Default::default();
        let mut parser = Parser::new(text);
        let gen = parser.gen_ast();
        match gen {
            Ok(ast) => {
                for node in ast.iter() {
                    ret.push(self.interpret_node(node));
                }
            }
            Err(e) => ret.push(Err(e)),
        }
        ret
    }
}

type InterpreterReturn = Result<Value, Error>;
type Context = HashMap<String, Value>;
trait Eval {
    fn eval(&self, ctx: &Context) -> InterpreterReturn;
}

impl Eval for AstNode {
    fn eval(&self, ctx: &Context) -> InterpreterReturn {
        match self {
            Self::Expression(expr) => expr.eval(ctx),
        }
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: &Context) -> InterpreterReturn {
        Ok(match self {
            Self::Add(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? + rhs.eval(ctx)?.int()?),
            Self::Subtract(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? - rhs.eval(ctx)?.int()?),
            Self::Term(term) => term.eval(ctx)?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: &Context) -> InterpreterReturn {
        Ok(match self {
            Self::Multiply(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? * rhs.eval(ctx)?.int()?),
            Self::Divide(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? / rhs.eval(ctx)?.int()?),
            Self::Factor(factor) => factor.eval(ctx)?,
        })
    }
}

impl Eval for Factor {
    fn eval(&self, ctx: &Context) -> InterpreterReturn {
        match self {
            Self::Literal(lit) => lit.eval(ctx),
            Self::Expression(expr) => expr.eval(ctx),
            Self::Negate(factor) => factor.eval(ctx),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _: &Context) -> InterpreterReturn {
        match self {
            Self::IntLiteral(int) => Ok(Value::Int(*int)),
            Self::StringLiteral(string) => Ok(Value::String(string.to_owned())),
        }
    }
}
