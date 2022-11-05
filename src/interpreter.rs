use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::parser::Parser;

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Function { func: Rc<FunctionType> },
    Int(i32),
    String(String),
    None,
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function { .. } => write!(f, "FunctionObject"),
            Self::Int(int) => write!(f, "{}", int),
            Self::String(string) => write!(f, "{}", string),
            Self::None => write!(f, "NoneType"),
        }
    }
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

    fn string(&self) -> Result<&String, RuntimeError> {
        if let Self::String(s) = self {
            Ok(s)
        } else {
            Err(RuntimeError::Error(
                format!("Expected integer type, found {} instead", self.get_type()).to_owned(),
            ))
        }
    }

    fn get_type(&self) -> &'static str {
        match self {
            RuntimeValue::Function { .. } => "Function",
            RuntimeValue::Int(_) => "Int",
            RuntimeValue::String(_) => "String",
            RuntimeValue::None => "NoneType",
        }
    }
}

#[derive(Debug)]
enum FunctionType {
    Lambda {
        parent_scope: Rc<Context>,
        parameters: Vec<Identifier>,
        body: Block,
    },
    Intrinsic {
        kind: IntrinsicFunction,
    },
}

#[derive(Debug)]
enum IntrinsicFunction {
    Print,
    TypeOf,
    Debug,
}

pub struct Interpreter {
    context: Rc<Context>,
}
impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            context: Rc::new(Context::init_global()),
        }
    }
    fn interpret_node(&mut self, node: &AstNode) -> InterpreterReturn {
        node.eval(self.context.clone())
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

#[derive(Clone)]
struct Context {
    data: RefCell<HashMap<String, RuntimeValue>>,
    parent: Option<Rc<Self>>,
}
impl Context {
    fn init_global() -> Self {
        let global = Context {
            data: Default::default(),
            parent: None,
        };
        global.insert(
            "print".to_string(),
            RuntimeValue::Function {
                func: Rc::new(FunctionType::Intrinsic {
                    kind: IntrinsicFunction::Print,
                }),
            },
        );
        global.insert(
            "typeof".to_string(),
            RuntimeValue::Function {
                func: Rc::new(FunctionType::Intrinsic {
                    kind: IntrinsicFunction::TypeOf,
                }),
            },
        );
        global.insert(
            "debug".to_string(),
            RuntimeValue::Function {
                func: Rc::new(FunctionType::Intrinsic {
                    kind: IntrinsicFunction::Debug,
                }),
            },
        );

        global
    }

    fn contains(&self, key: &String) -> bool {
        if self.data.borrow().contains_key(key) {
            true
        } else if let Some(parent) = &self.parent {
            parent.contains(key)
        } else {
            false
        }
    }

    fn contains_in_scope(&self, key: &String) -> bool {
        self.data.borrow().contains_key(key)
    }

    fn insert(&self, key: String, val: RuntimeValue) -> Option<RuntimeValue> {
        self.data.borrow_mut().insert(key, val)
    }

    fn update(&self, key: String, val: RuntimeValue) -> Option<RuntimeValue> {
        if self.contains_in_scope(&key) {
            self.insert(key, val)
        } else if let Some(parent) = &self.parent {
            parent.update(key, val)
        } else {
            None
        }
    }

    fn get(&self, key: &String) -> Option<RuntimeValue> {
        self.data.borrow().get(key).cloned().or_else(|| {
            if let Some(parent) = &self.parent {
                parent.get(key)
            } else {
                None
            }
        })
    }

    fn new(parent: Rc<Self>) -> Self {
        Context {
            data: Default::default(),
            parent: Some(parent),
        }
    }
}

impl Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Context {{ identifiers: {:?}, parent: {:?} }}",
            self.data.borrow().keys(),
            self.parent
        )
    }
}

trait Eval {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn;
}

impl Eval for AstNode {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Statement(statement) => statement.eval(ctx.clone()),
            // Self::Expression(expr) => expr.eval(ctx.clone()),
            // Self::Block(block) => block.eval(ctx.clone()),
        }
    }
}

impl Eval for Statement {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Declaration(lhs, rhs) => {
                if !ctx.contains_in_scope(&lhs.name) {
                    let value = rhs.eval(ctx.clone())?;
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
                    let value = rhs.eval(ctx.clone())?;
                    ctx.update(lhs.name().clone(), value)
                        .expect("could not update value");
                    Ok(RuntimeValue::None)
                } else {
                    Err(RuntimeError::Error(
                        format!("Variable {} has not been declared in scope", lhs.name())
                            .to_owned(),
                    )
                    .into())
                }
            }
            Self::Expression(expr) => expr.eval(ctx.clone()),
        }
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        Ok(match self {
            Self::Add(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx.clone())?.int()? + rhs.eval(ctx.clone())?.int()?)
            }
            Self::Subtract(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx.clone())?.int()? - rhs.eval(ctx.clone())?.int()?)
            }
            Self::Term(term) => term.eval(ctx.clone())?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        Ok(match self {
            Self::Multiply(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx.clone())?.int()? * rhs.eval(ctx.clone())?.int()?)
            }
            Self::Divide(lhs, rhs) => {
                RuntimeValue::Int(lhs.eval(ctx.clone())?.int()? / rhs.eval(ctx.clone())?.int()?)
            }
            Self::Concatenate(lhs, rhs) => RuntimeValue::String(format!(
                "{}{}",
                lhs.eval(ctx.clone())?.string()?,
                rhs.eval(ctx.clone())?.string()?
            )),
            Self::Factor(factor) => factor.eval(ctx.clone())?,
        })
    }
}

impl Eval for Factor {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Literal(lit) => lit.eval(ctx.clone()),
            Self::Expression(expr) => expr.eval(ctx.clone()),
            Self::Negate(factor) => factor.eval(ctx.clone()),
            Self::Variable(identifier) => identifier.eval(ctx.clone()),
            Self::Lambda(param, body) => Ok(RuntimeValue::Function {
                func: Rc::new(FunctionType::Lambda {
                    parent_scope: ctx,
                    parameters: param.to_owned(),
                    body: body.to_owned(),
                }),
            }),
            Self::FunctionCall(fun, args) => match fun.eval(ctx.clone())? {
                RuntimeValue::Function { func: function } => match function.borrow() {
                    FunctionType::Lambda {
                        parent_scope,
                        parameters,
                        body,
                    } => {
                        let scope = Context::new(parent_scope.clone());
                        if parameters.len() != args.len() {
                            Err(RuntimeError::Error(format!(
                                "Function expects {} argument but {} were provided",
                                parameters.len(),
                                args.len()
                            )))?
                        }
                        for (ident, val) in parameters.iter().zip(args.iter()) {
                            scope.insert(ident.name.to_owned(), val.eval(ctx.clone())?);
                        }
                        let rc = Rc::new(scope);
                        body.eval(rc.clone())?;
                        Ok(rc
                            .get(&"return".to_string())
                            .unwrap_or(RuntimeValue::None)
                            .to_owned())
                    }
                    FunctionType::Intrinsic { kind } => match kind {
                        IntrinsicFunction::Print => {
                            for arg in args {
                                print!("{} ", arg.eval(ctx.clone())?)
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
                                    args[0].eval(ctx.clone())?.get_type().to_owned(),
                                ))
                            }
                        }
                        IntrinsicFunction::Debug => {
                            for arg in args {
                                print!("{:?} ", arg.eval(ctx.clone())?)
                            }
                            println!("");
                            Ok(RuntimeValue::None)
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

impl Eval for Block {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        let Self(vec) = self;
        for node in vec {
            node.eval(ctx.clone())?;
        }
        Ok(RuntimeValue::None)
    }
}

impl Eval for Literal {
    fn eval(&self, _: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::IntLiteral(int) => Ok(RuntimeValue::Int(*int)),
            Self::StringLiteral(string) => Ok(RuntimeValue::String(string.to_owned())),
        }
    }
}

impl Eval for Identifier {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match ctx.get(&self.name) {
            Some(val) => Ok(val.clone()),
            None => Err(RuntimeError::Error(format!("Unknown identifier `{}`", self.name)).into()),
        }
    }
}
