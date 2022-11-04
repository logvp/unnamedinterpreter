use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::parser::Parser;

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Function(Rc<FunctionType>),
    Int(i32),
    String(String),
    None,
}

impl RuntimeValue {
    fn int(&self) -> Result<i32, RuntimeError> {
        if let Self::Int(int) = self {
            Ok(*int)
        } else {
            Err(RuntimeError::Error(
                format!("Expected integer type, found {} instead", self.get_type()).to_owned(),
            ))
        }
    }

    fn get_type(&self) -> &'static str {
        match self {
            RuntimeValue::Function(_) => "Function",
            RuntimeValue::Int(_) => "Int",
            RuntimeValue::String(_) => "String",
            RuntimeValue::None => "NoneType",
        }
    }
}

#[derive(Debug)]
enum FunctionType {
    Function {
        name: Identifier,
        arguments: Vec<RuntimeValue>,
        body: AstNode,
    },
    Intrinsic {
        kind: IntrinsicFunction,
    },
}

#[derive(Debug)]
enum IntrinsicFunction {
    Print,
    TypeOf,
}

impl FunctionType {
    fn exec(&self, args: (), ctx: &mut Context) {}
}

pub struct Interpreter {
    context: Context,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Context::init_global(),
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

type InterpreterReturn = Result<RuntimeValue, Error>;
// type Context = HashMap<String, Value>;

#[derive(Debug)]
struct Context {
    data: HashMap<String, RuntimeValue>,
    parent: Option<Box<Self>>,
}
impl Context {
    fn init_global() -> Self {
        let mut global = Context {
            data: Default::default(),
            parent: None,
        };
        global.insert(
            "print".to_string(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic {
                kind: IntrinsicFunction::Print,
            })),
        );
        global.insert(
            "typeof".to_string(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic {
                kind: IntrinsicFunction::TypeOf,
            })),
        );

        global
    }

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

    fn contains_in_scope(&self, key: &String) -> bool {
        self.data.contains_key(key)
    }

    fn insert(&mut self, key: String, val: RuntimeValue) -> Option<RuntimeValue> {
        self.data.insert(key, val)
    }

    fn get(&self, key: &String) -> Option<&RuntimeValue> {
        self.data.get(key)
    }

    fn enter_context(self) -> Self {
        Context {
            data: Default::default(),
            parent: Some(Box::new(self)),
        }
    }

    fn exit_context(self) -> Result<Self, RuntimeError> {
        if let Some(parent) = self.parent {
            Ok(*parent)
        } else {
            Err(RuntimeError::Error(
                "Could not exit scope. Likely in Global".to_owned(),
            ))
        }
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
                if !ctx.contains_in_scope(&lhs.name) {
                    let value = rhs.eval(ctx)?;
                    ctx.insert(lhs.name.clone(), value);
                    Ok(RuntimeValue::None)
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
                    Ok(RuntimeValue::None)
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
            Self::Add(lhs, rhs) => RuntimeValue::Int(lhs.eval(ctx)?.int()? + rhs.eval(ctx)?.int()?),
            Self::Subtract(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx)?.int()? - rhs.eval(ctx)?.int()?)
            }
            Self::Term(term) => term.eval(ctx)?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: &mut Context) -> InterpreterReturn {
        Ok(match self {
            Self::Multiply(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx)?.int()? * rhs.eval(ctx)?.int()?)
            }
            Self::Divide(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx)?.int()? / rhs.eval(ctx)?.int()?)
            }
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
            Self::Function(fun, args) => match fun.eval(ctx)? {
                RuntimeValue::Function(function) => match function.borrow() {
                    FunctionType::Function {
                        name,
                        arguments,
                        body,
                    } => todo!("Function calls not implemented yet"),
                    FunctionType::Intrinsic { kind } => match kind {
                        IntrinsicFunction::Print => {
                            for arg in args {
                                print!("{:?} ", arg.eval(ctx)?)
                            }
                            println!("");
                            Ok(RuntimeValue::None)
                        }
                        IntrinsicFunction::TypeOf => {
                            if args.len() != 1 {
                                Err(RuntimeError::Error(format!(
                                    "TypeOf intrinsic expects 1 argument found {} instead",
                                    args.len()
                                ))
                                .into())
                            } else {
                                Ok(RuntimeValue::String(
                                    args[0].eval(ctx)?.get_type().to_owned(),
                                ))
                            }
                        }
                    },
                },
                _ => Err(RuntimeError::Error(format!(
                    "Function call expected Function found {} instead",
                    fun
                ))
                .into()),
            },
        }
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> InterpreterReturn {
        match self {
            Self::IntLiteral(int) => Ok(RuntimeValue::Int(*int)),
            Self::StringLiteral(string) => Ok(RuntimeValue::String(string.to_owned())),
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
