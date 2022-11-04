use std::collections::HashMap;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::parser::Parser;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    String(String),
    None,
}

impl Value {
    fn int(&self) -> Result<i32, RuntimeError> {
        if let Self::Int(int) = self {
            Ok(*int)
        } else {
            Err(RuntimeError::Error("Expected integer type".to_owned()))
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
        node.eval(&mut self.context)
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
// type Context = HashMap<String, Value>;

#[derive(Default, Debug)]
struct Context {
    data: HashMap<String, Value>,
    parent: Option<Box<Self>>,
}
impl Context {
    fn contains(&self, key: &String) -> bool {
        if self.data.contains_key(key) {
            true
        } else {
            if let Some(ctx) = &self.parent {
                ctx.contains(key)
            } else {
                false
            }
        }
    }

    fn insert(&mut self, key: String, val: Value) -> Option<Value> {
        self.data.insert(key, val)
    }

    fn get(&self, key: &String) -> Option<&Value> {
        self.data.get(key)
    }
}

trait Eval {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn;
}

impl Eval for AstNode {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        match self {
            Self::Statement(statement) => statement.eval(ctx),
            // Self::Expression(expr) => expr.eval(ctx),
        }
    }
}

impl Eval for Statement {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        match self {
            Self::Declaration(lhs, rhs) => {
                if !ctx.contains(&lhs.name) {
                    let value = rhs.eval(ctx)?;
                    ctx.insert(lhs.name.clone(), value);
                    Ok(Value::None)
                } else {
                    Err(RuntimeError::Error(
                        format!("Variable {} was already declared in scope", lhs.name).to_owned(),
                    ))?
                }
            }
            Self::Assignment(lhs, rhs) => {
                if ctx.contains(lhs.name()) {
                    let value = rhs.eval(ctx)?;
                    ctx.insert(lhs.name().clone(), value);
                    Ok(Value::None)
                } else {
                    Err(RuntimeError::Error(
                        format!("Variable {} has not been declared in scope", lhs.name())
                            .to_owned(),
                    )
                    .into())
                }
            }
            Self::Expression(expr) => expr.eval(ctx),
        }
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        Ok(match self {
            Self::Add(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? + rhs.eval(ctx)?.int()?),
            Self::Subtract(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? - rhs.eval(ctx)?.int()?),
            Self::Term(term) => term.eval(ctx)?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        Ok(match self {
            Self::Multiply(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? * rhs.eval(ctx)?.int()?),
            Self::Divide(lhs, rhs) => Value::Int(lhs.eval(ctx)?.int()? / rhs.eval(ctx)?.int()?),
            Self::Factor(factor) => factor.eval(ctx)?,
        })
    }
}

impl Eval for Factor {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        match self {
            Self::Literal(lit) => lit.eval(ctx),
            Self::Expression(expr) => expr.eval(ctx),
            Self::Negate(factor) => factor.eval(ctx),
            Self::Variable(identifier) => identifier.eval(ctx),
            Self::Function(fun, args) => todo!("Function call not implemented"),
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> InterpreterReturn {
        match self {
            Self::IntLiteral(int) => Ok(Value::Int(*int)),
            Self::StringLiteral(string) => Ok(Value::String(string.to_owned())),
        }
    }
}

impl Eval for Identifier {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        match ctx.get(&self.name) {
            Some(val) => Ok(val.clone()),
            None => Err(RuntimeError::Error(format!("Unknown identifier `{}`", self.name)).into()),
        }
    }
}
