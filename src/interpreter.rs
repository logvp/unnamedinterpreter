use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::parser::Parser;

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Function(Rc<Function>),
    Integer(i32),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug)]
pub enum RuntimeType {
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
                Self::Function => "Function",
                Self::Integer => "Integer",
                Self::String => "String",
                Self::Boolean => "Boolean",
                Self::None => "NoneType",
            }
        )
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function { .. } => write!(f, "FunctionObject"),
            Self::Integer(int) => write!(f, "{}", int),
            Self::String(string) => write!(f, "{:?}", string),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::None => write!(f, "NoneType"),
        }
    }
}
impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Integer(a) => {
                if let Self::Integer(b) = other {
                    *a == *b
                } else {
                    false
                }
            }
            Self::String(a) => {
                if let Self::String(b) = other {
                    *a == *b
                } else {
                    false
                }
            }
            Self::Boolean(a) => {
                if let Self::Boolean(b) = other {
                    *a == *b
                } else {
                    false
                }
            }
            Self::Function(a) => {
                // Two functions are equal iff they are aliases of each other
                if let Self::Function(b) = other {
                    Rc::ptr_eq(a, b)
                } else {
                    false
                }
            }
            Self::None => {
                matches!(other, Self::None)
            }
        }
    }
}
impl RuntimeValue {
    fn int(&self) -> Result<i32, RuntimeError> {
        if let Self::Integer(int) = self {
            Ok(*int)
        } else {
            Err(RuntimeError::ExpectedButFound(
                RuntimeType::Integer,
                self.get_type(),
            ))
        }
    }

    fn string(&self) -> Result<&String, RuntimeError> {
        if let Self::String(string) = self {
            Ok(string)
        } else {
            Err(RuntimeError::ExpectedButFound(
                RuntimeType::String,
                self.get_type(),
            ))
        }
    }

    fn boolean(&self) -> Result<bool, RuntimeError> {
        if let Self::Boolean(boolean) = self {
            Ok(*boolean)
        } else {
            Err(RuntimeError::ExpectedButFound(
                RuntimeType::Boolean,
                self.get_type(),
            ))
        }
    }

    fn get_type(&self) -> RuntimeType {
        match self {
            Self::Function { .. } => RuntimeType::Function,
            Self::Integer(_) => RuntimeType::Integer,
            Self::String(_) => RuntimeType::String,
            Self::Boolean(_) => RuntimeType::Boolean,
            Self::None => RuntimeType::None,
        }
    }
}

#[derive(Debug)]
pub struct Function(FunctionType);

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
    fn interpret_node(&mut self, node: &AstNode) -> Result<RuntimeValue, Error> {
        node.eval(self.context.clone())
    }
    pub fn interpret(&mut self, text: String) -> Vec<Result<RuntimeValue, Error>> {
        let mut ret: Vec<Result<RuntimeValue, Error>> = Default::default();
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

#[derive(Clone, Debug)]
struct Variable {
    val: RuntimeValue,
    is_const: bool,
}

#[derive(Clone)]
struct Context {
    data: RefCell<HashMap<String, Variable>>,
    parent: Option<Rc<Self>>,
}
impl Context {
    fn init_global() -> Self {
        let global = Context {
            data: Default::default(),
            parent: None,
        };
        global.declare(
            "print".to_string(),
            RuntimeValue::Function(Rc::new(Function(FunctionType::Intrinsic {
                kind: IntrinsicFunction::Print,
            }))),
            true,
        );
        global.declare(
            "typeof".to_string(),
            RuntimeValue::Function(Rc::new(Function(FunctionType::Intrinsic {
                kind: IntrinsicFunction::TypeOf,
            }))),
            true,
        );
        global.declare(
            "debug".to_string(),
            RuntimeValue::Function(Rc::new(Function(FunctionType::Intrinsic {
                kind: IntrinsicFunction::Debug,
            }))),
            true,
        );

        global
    }

    fn is_const(&self, key: &String) -> Option<bool> {
        if self.data.borrow().contains_key(key) {
            Some(self.data.borrow().get(key).unwrap().is_const)
        } else if let Some(parent) = &self.parent {
            parent.is_const(key)
        } else {
            None
        }
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

    fn declare(&self, key: String, val: RuntimeValue, is_const: bool) {
        self.data
            .borrow_mut()
            .insert(key, Variable { val, is_const })
            .ok_or(())
            .expect_err("Cannot redeclare variable in same scope");
    }

    fn update(&self, key: String, val: RuntimeValue) -> Result<(), RuntimeError> {
        if self.contains_in_scope(&key) {
            if self.is_const(&key).unwrap() {
                Err(RuntimeError::ConstReassignment(key))
            } else {
                self.data.borrow_mut().insert(
                    key,
                    Variable {
                        val,
                        is_const: false,
                    },
                );
                Ok(())
            }
        } else if let Some(parent) = &self.parent {
            parent.update(key, val)
        } else {
            Err(RuntimeError::UnknownIdentifier(key))
        }
    }

    fn get(&self, key: &String) -> Option<Variable> {
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
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error>;
}

impl Eval for AstNode {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        match self {
            Self::Statement(statement) => statement.eval(ctx),
            Self::Expression(expr) => expr.eval(ctx),
        }
    }
}

impl Eval for Statement {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        match self {
            Self::Declaration(lhs, rhs, is_const) => {
                if !ctx.contains_in_scope(&lhs.name) {
                    let value = rhs.eval(ctx.clone())?;
                    ctx.declare(lhs.name.clone(), value, *is_const);
                } else {
                    Err(RuntimeError::VariableRedeclaration(lhs.name.clone()))?
                }
            }
            Self::Assignment(lhs, rhs) => {
                if ctx.contains(lhs.name().unwrap()) {
                    let value = rhs.eval(ctx.clone())?;
                    ctx.update(lhs.name().unwrap().clone(), value)?;
                } else {
                    Err(RuntimeError::UnknownIdentifier(lhs.name().unwrap().clone()))?
                }
            }
            Self::Expression(expr) => {
                expr.eval(ctx)?;
            }
        };
        Ok(RuntimeValue::None)
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        Ok(match self {
            Self::Add(lhs, rhs) => {
                RuntimeValue::Integer(lhs.eval(ctx.clone())?.int()? + rhs.eval(ctx)?.int()?)
            }
            Self::Subtract(lhs, rhs) => {
                RuntimeValue::Integer(lhs.eval(ctx.clone())?.int()? - rhs.eval(ctx)?.int()?)
            }
            Self::Compare(lhs, rhs, op) => RuntimeValue::Boolean(match op {
                Comparison::Equal => lhs.eval(ctx.clone())? == rhs.eval(ctx)?,
                Comparison::NotEqual => lhs.eval(ctx.clone())? != rhs.eval(ctx)?,
                Comparison::LessThan => lhs.eval(ctx.clone())?.int()? < rhs.eval(ctx)?.int()?,
                Comparison::LessEqual => lhs.eval(ctx.clone())?.int()? <= rhs.eval(ctx)?.int()?,
                Comparison::GreaterThan => lhs.eval(ctx.clone())?.int()? > rhs.eval(ctx)?.int()?,
                Comparison::GreaterEqual => {
                    lhs.eval(ctx.clone())?.int()? >= rhs.eval(ctx)?.int()?
                }
            }),
            Self::IfElse(cond, body, else_block) => {
                if cond.eval(ctx.clone())?.boolean()? {
                    body.eval(ctx)?
                } else {
                    else_block.eval(ctx)?
                }
            }
            Self::While(cond, body) => {
                let mut ret = RuntimeValue::None;
                while cond.eval(ctx.clone())?.boolean()? {
                    ret = body.eval(ctx.clone())?;
                }
                ret
            }
            Self::Term(term) => term.eval(ctx)?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        match self {
            Self::Multiply(lhs, rhs) => Ok(RuntimeValue::Integer(
                lhs.eval(ctx.clone())?.int()? * rhs.eval(ctx)?.int()?,
            )),
            Self::Divide(lhs, rhs) => Ok(RuntimeValue::Integer(
                lhs.eval(ctx.clone())?.int()? / rhs.eval(ctx)?.int()?,
            )),
            Self::Concatenate(lhs, rhs) => Ok(RuntimeValue::String(format!(
                "{}{}",
                lhs.eval(ctx.clone())?.string()?,
                rhs.eval(ctx)?.string()?
            ))),
            Self::Factor(factor) => factor.eval(ctx),
        }
    }
}

impl Eval for Factor {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        match self {
            Self::Literal(lit) => lit.eval(ctx),
            Self::Expression(expr) => expr.eval(ctx),
            Self::Negate(factor) => Ok(RuntimeValue::Integer(-factor.eval(ctx)?.int()?)),
            Self::Variable(identifier) => identifier.eval(ctx),
            Self::Lambda(param, body) => Ok(RuntimeValue::Function(Rc::new(Function(
                FunctionType::Lambda {
                    parent_scope: ctx,
                    parameters: param.to_owned(),
                    body: body.to_owned(),
                },
            )))),
            Self::FunctionCall(fun, args) => match fun.eval(ctx.clone())? {
                RuntimeValue::Function(f) => {
                    let Function(function) = f.borrow();
                    match function {
                        FunctionType::Lambda {
                            parent_scope,
                            parameters,
                            body,
                        } => {
                            let scope = Context::new(parent_scope.clone());
                            if parameters.len() != args.len() {
                                Err(RuntimeError::ExpectedArgumentsFound(
                                    parameters.len(),
                                    args.len(),
                                ))?
                            }
                            // Bind arguments to parameter names
                            for (ident, val) in parameters.iter().zip(args.iter()) {
                                scope.declare(ident.name.to_owned(), val.eval(ctx.clone())?, false);
                            }

                            body.eval(Rc::new(scope))
                        }
                        FunctionType::Intrinsic { kind } => match kind {
                            IntrinsicFunction::Print => {
                                for arg in args {
                                    print!("{} ", arg.eval(ctx.clone())?)
                                }
                                println!();
                                Ok(RuntimeValue::None)
                            }
                            IntrinsicFunction::TypeOf => {
                                if args.len() != 1 {
                                    Err(RuntimeError::ExpectedArgumentsFound(1, args.len()).into())
                                } else {
                                    Ok(RuntimeValue::String(format!(
                                        "{}",
                                        args[0].eval(ctx)?.get_type()
                                    )))
                                }
                            }
                            IntrinsicFunction::Debug => {
                                for arg in args {
                                    print!("{:?} ", arg.eval(ctx.clone())?)
                                }
                                println!();
                                Ok(RuntimeValue::None)
                            }
                        },
                    }
                }
                x => {
                    Err(RuntimeError::ExpectedButFound(RuntimeType::Function, x.get_type()).into())
                }
            },
        }
    }
}

impl Eval for Block {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        let Self(vec) = self;
        let mut ret = RuntimeValue::None;
        for node in vec {
            ret = node.eval(ctx.clone())?;
        }
        Ok(ret)
    }
}

impl Eval for Literal {
    fn eval(&self, _: Rc<Context>) -> Result<RuntimeValue, Error> {
        match self {
            Self::Integer(int) => Ok(RuntimeValue::Integer(*int)),
            Self::String(string) => Ok(RuntimeValue::String(string.to_owned())),
            Self::Boolean(boolean) => Ok(RuntimeValue::Boolean(*boolean)),
        }
    }
}

impl Eval for Identifier {
    fn eval(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        match ctx.get(&self.name) {
            Some(Variable { val, .. }) => Ok(val),
            None => Err(RuntimeError::UnknownIdentifier(self.name.to_owned()).into()),
        }
    }
}
