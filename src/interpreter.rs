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
    Function(Rc<FunctionType>), // TODO: Fix this somehow
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
            Self::String(string) => write!(f, "{}", string),
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
                if let Self::None = other {
                    true
                } else {
                    false
                }
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
        global.insert(
            "debug".to_string(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic {
                kind: IntrinsicFunction::Debug,
            })),
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

    fn get_from_local(&self, key: &String) -> Option<RuntimeValue> {
        self.data.borrow().get(key).cloned()
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
            Self::Expression(expr) => expr.eval(ctx.clone()),
            // Self::Block(block) => block.eval(ctx.clone()),
        }
    }
}

impl Eval for Statement {
    // TODO: Statements should not return values but this works until expressions are allowed as AstNode
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Declaration(lhs, rhs) => {
                if !ctx.contains_in_scope(&lhs.name) {
                    let value = rhs.eval(ctx.clone())?;
                    ctx.insert(lhs.name.clone(), value);
                    Ok(RuntimeValue::None)
                } else {
                    Err(RuntimeError::VariableAlreadyDeclared(lhs.name.clone()))?
                }
            }
            Self::Assignment(lhs, rhs) => {
                if ctx.contains(lhs.name().unwrap()) {
                    let value = rhs.eval(ctx.clone())?;
                    ctx.update(lhs.name().unwrap().clone(), value)
                        .expect("could not update value");
                    Ok(RuntimeValue::None)
                } else {
                    Err(RuntimeError::UnknownIdentifier(lhs.name().unwrap().clone()).into())
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
                RuntimeValue::Integer(lhs.eval(ctx.clone())?.int()? + rhs.eval(ctx.clone())?.int()?)
            }
            Self::Subtract(lhs, rhs) => {
                RuntimeValue::Integer(lhs.eval(ctx.clone())?.int()? - rhs.eval(ctx.clone())?.int()?)
            }
            Self::Compare(lhs, rhs, op) => RuntimeValue::Boolean(match op {
                Comparison::Equal => lhs.eval(ctx.clone())? == rhs.eval(ctx.clone())?,
                Comparison::NotEqual => lhs.eval(ctx.clone())? != rhs.eval(ctx.clone())?,
                Comparison::LessThan => {
                    lhs.eval(ctx.clone())?.int()? < rhs.eval(ctx.clone())?.int()?
                }
                Comparison::LessEqual => {
                    lhs.eval(ctx.clone())?.int()? <= rhs.eval(ctx.clone())?.int()?
                }
                Comparison::GreaterThan => {
                    lhs.eval(ctx.clone())?.int()? > rhs.eval(ctx.clone())?.int()?
                }
                Comparison::GreaterEqual => {
                    lhs.eval(ctx.clone())?.int()? >= rhs.eval(ctx.clone())?.int()?
                }
            }),
            Self::IfElse(cond, body, else_block) => {
                if cond.eval(ctx.clone())?.boolean()? {
                    body.eval(ctx.clone())?
                } else {
                    else_block.eval(ctx.clone())?
                }
            }
            Self::While(cond, body) => {
                let mut ret = RuntimeValue::None;
                while cond.eval(ctx.clone())?.boolean()? {
                    ret = body.eval(ctx.clone())?;
                }
                ret
            }
            Self::Term(term) => term.eval(ctx.clone())?,
        })
    }
}

impl Eval for Term {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Multiply(lhs, rhs) => Ok(RuntimeValue::Integer(
                lhs.eval(ctx.clone())?.int()? * rhs.eval(ctx.clone())?.int()?,
            )),
            Self::Divide(lhs, rhs) => Ok(RuntimeValue::Integer(
                lhs.eval(ctx.clone())?.int()? / rhs.eval(ctx.clone())?.int()?,
            )),
            Self::Concatenate(lhs, rhs) => Ok(RuntimeValue::String(format!(
                "{}{}",
                lhs.eval(ctx.clone())?.string()?,
                rhs.eval(ctx.clone())?.string()?
            ))),
            Self::Factor(factor) => factor.eval(ctx.clone()),
        }
    }
}

impl Eval for Factor {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::Literal(lit) => lit.eval(ctx.clone()),
            Self::Expression(expr) => expr.eval(ctx.clone()),
            Self::Negate(factor) => Ok(RuntimeValue::Integer(-factor.eval(ctx.clone())?.int()?)),
            Self::Variable(identifier) => identifier.eval(ctx.clone()),
            Self::Lambda(param, body) => {
                Ok(RuntimeValue::Function(Rc::new(FunctionType::Lambda {
                    parent_scope: ctx,
                    parameters: param.to_owned(),
                    body: body.to_owned(),
                })))
            }
            Self::FunctionCall(fun, args) => match fun.eval(ctx.clone())? {
                RuntimeValue::Function(function) => match function.borrow() {
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
                        for (ident, val) in parameters.iter().zip(args.iter()) {
                            scope.insert(ident.name.to_owned(), val.eval(ctx.clone())?);
                        }
                        let rc = Rc::new(scope);
                        body.eval(rc.clone())?;
                        Ok(rc
                            // Fixes value being returned from outer scope
                            // TODO: don't let functions modify return values of outer scope
                            .get_from_local(&"return".to_string())
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
                                Err(RuntimeError::ExpectedArgumentsFound(1, args.len()).into())
                            } else {
                                Ok(RuntimeValue::String(format!(
                                    "{}",
                                    args[0].eval(ctx.clone())?.get_type()
                                )))
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
                x => {
                    Err(RuntimeError::ExpectedButFound(RuntimeType::Function, x.get_type()).into())
                }
            },
        }
    }
}

impl Eval for Block {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        let Self(vec) = self;
        let mut ret = RuntimeValue::None;
        for node in vec {
            ret = node.eval(ctx.clone())?;
        }
        Ok(ret)
    }
}

impl Eval for Literal {
    fn eval(&self, _: Rc<Context>) -> InterpreterReturn {
        match self {
            Self::IntLiteral(int) => Ok(RuntimeValue::Integer(*int)),
            Self::StringLiteral(string) => Ok(RuntimeValue::String(string.to_owned())),
            Self::BooleanLiteral(boolean) => Ok(RuntimeValue::Boolean(*boolean)),
        }
    }
}

impl Eval for Identifier {
    fn eval(&self, ctx: Rc<Context>) -> InterpreterReturn {
        match ctx.get(&self.name) {
            Some(val) => Ok(val.clone()),
            None => Err(RuntimeError::UnknownIdentifier(self.name.to_owned()).into()),
        }
    }
}
