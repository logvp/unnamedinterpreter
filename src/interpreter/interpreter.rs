use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::parser::Parser;

use super::instrinsics::IntrinsicFunction;

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Object(Object),
    Function(Rc<FunctionType>),
    Integer(i32),
    String(String),
    Boolean(bool),
    None,
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

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Object(ctx) => write!(f, "{:?}", ctx),
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
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Object(a), Self::Object(b)) => a == b,
            (Self::Function(a), Self::Function(b)) => {
                // Two functions are equal iff they are aliases of each other
                Rc::ptr_eq(a, b)
            }
            (Self::None, Self::None) => true,
            _ => false,
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

    fn string(&self) -> Result<&str, RuntimeError> {
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

    pub(super) fn get_type(&self) -> RuntimeType {
        match self {
            Self::Object(_) => RuntimeType::Object,
            Self::Function(_) => RuntimeType::Function,
            Self::Integer(_) => RuntimeType::Integer,
            Self::String(_) => RuntimeType::String,
            Self::Boolean(_) => RuntimeType::Boolean,
            Self::None => RuntimeType::None,
        }
    }
}

#[derive(Debug)]
pub enum FunctionType {
    Lambda(Lambda),
    Intrinsic(IntrinsicFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object(Rc<Context>);

#[derive(Debug)]
pub struct Lambda {
    parent_scope: Rc<Context>,
    parameters: Rc<[Identifier]>,
    body: Block,
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
        node.eval(Rc::clone(&self.context))
    }
    pub fn interpret(
        &mut self,
        text: &str,
        filename: Option<Rc<str>>,
    ) -> Vec<Result<RuntimeValue, Error>> {
        let mut ret: Vec<Result<RuntimeValue, Error>> = Default::default();
        let mut parser = {
            match Parser::new(text, filename) {
                Ok(parser) => parser,
                Err(e) => {
                    ret.push(Err(e));
                    return ret;
                }
            }
        };
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

#[derive(Clone, Debug, PartialEq)]
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
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::Print))),
            true,
        );
        global.declare(
            "typeof".to_string(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::TypeOf))),
            true,
        );
        global.declare(
            "debug".to_string(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::Debug))),
            true,
        );

        global
    }

    fn is_const(&self, key: &str) -> Option<bool> {
        if self.data.borrow().contains_key(key) {
            Some(self.data.borrow().get(key).unwrap().is_const)
        } else if let Some(parent) = &self.parent {
            parent.is_const(key)
        } else {
            None
        }
    }

    fn contains(&self, key: &str) -> bool {
        if self.data.borrow().contains_key(key) {
            true
        } else if let Some(parent) = &self.parent {
            parent.contains(key)
        } else {
            false
        }
    }

    fn contains_in_scope(&self, key: &str) -> bool {
        self.data.borrow().contains_key(key)
    }

    fn declare(&self, key: String, val: RuntimeValue, is_const: bool) {
        let None = self.data.borrow_mut().insert(key, Variable { val, is_const })
        else {
            panic!("Cannot redeclare variable in same scope")
        };
    }

    fn update(&self, key: String, val: RuntimeValue) -> Result<(), RuntimeError> {
        if self.contains_in_scope(&key) {
            if self.is_const(&key).unwrap() {
                Err(RuntimeError::ConstReassignment(key))
            } else {
                let var = Variable {
                    val,
                    is_const: false,
                };
                self.data.borrow_mut().insert(key, var);
                Ok(())
            }
        } else if let Some(parent) = &self.parent {
            parent.update(key, val)
        } else {
            Err(RuntimeError::UnknownIdentifier(key))
        }
    }

    fn get(&self, key: &str) -> Option<Variable> {
        self.data
            .borrow()
            .get(key)
            .cloned()
            .or(self.parent.as_ref().and_then(|p| p.get(key)))
    }

    fn new(parent: Rc<Self>) -> Self {
        Context {
            data: Default::default(),
            parent: Some(parent),
        }
    }
}
impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        *self.data.borrow() == *other.data.borrow() && {
            match (&self.parent, &other.parent) {
                (Some(a), Some(b)) => Rc::ptr_eq(a, b),
                (None, None) => true,
                _ => false,
            }
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
                    let value = rhs.eval(Rc::clone(&ctx))?;
                    ctx.declare(lhs.name.to_owned(), value, *is_const);
                } else {
                    Err(RuntimeError::VariableRedeclaration(lhs.name.to_owned()))?
                }
            }
            Self::Assignment(lhs, rhs) => {
                if ctx.contains(lhs.name().unwrap()) {
                    let value = rhs.eval(Rc::clone(&ctx))?;
                    ctx.update(lhs.name().unwrap().to_owned(), value)?;
                } else {
                    Err(RuntimeError::UnknownIdentifier(
                        lhs.name().unwrap().to_owned(),
                    ))?
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
            Self::Binary(op, lhs, rhs) => do_binary_operation(*op, lhs, rhs, ctx)?,
            Self::Unary(op, erand) => do_unary_operation(*op, erand, ctx)?,

            Self::IfElse(cond, body, else_block) => {
                if cond.eval(Rc::clone(&ctx))?.boolean()? {
                    body.eval(ctx)?
                } else {
                    else_block.eval(ctx)?
                }
            }
            Self::While(cond, body) => {
                let mut ret = RuntimeValue::None;
                while cond.eval(Rc::clone(&ctx))?.boolean()? {
                    ret = body.eval(Rc::clone(&ctx))?;
                }
                ret
            }
            Self::With(with, body) => {
                let arg = with.eval(ctx)?;
                if let RuntimeValue::Object(Object(obj_ctx)) = arg {
                    body.eval_with_context(obj_ctx)?
                } else {
                    Err(RuntimeError::ExpectedButFound(
                        RuntimeType::Object,
                        arg.get_type(),
                    ))?
                }
            }
            Self::New(block) => {
                let ctx = Rc::new(Context::new(ctx));
                block.eval_with_context(Rc::clone(&ctx))?;
                RuntimeValue::Object(Object(ctx))
            }
            Self::Literal(lit) => lit.eval(ctx)?,
            Self::Variable(identifier) => identifier.eval(ctx)?,
            Self::Block(block) => block.eval(ctx)?,
            Self::Lambda(param, body) => {
                RuntimeValue::Function(Rc::new(FunctionType::Lambda(Lambda {
                    parent_scope: ctx,
                    parameters: Rc::clone(param),
                    body: body.clone(),
                })))
            }
            Self::FunctionCall(fun, args) => do_function_call(
                fun.eval(Rc::clone(&ctx))?,
                args.iter()
                    .map(|x| x.eval(Rc::clone(&ctx)))
                    .collect::<Result<Vec<RuntimeValue>, _>>()?,
            )?,
        })
    }
}

fn do_binary_operation(
    op: BinaryOperator,
    lhs: &Expression,
    rhs: &Expression,
    ctx: Rc<Context>,
) -> Result<RuntimeValue, Error> {
    Ok(match op {
        BinaryOperator::Equal => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))? == rhs.eval(ctx)?)
        }
        BinaryOperator::NotEqual => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))? != rhs.eval(ctx)?)
        }
        BinaryOperator::LessThan => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))?.int()? < rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::LessEqual => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))?.int()? <= rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::GreaterThan => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))?.int()? > rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::GreaterEqual => {
            RuntimeValue::Boolean(lhs.eval(Rc::clone(&ctx))?.int()? >= rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::Add => {
            RuntimeValue::Integer(lhs.eval(Rc::clone(&ctx))?.int()? + rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::Subtract => {
            RuntimeValue::Integer(lhs.eval(Rc::clone(&ctx))?.int()? - rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::Multiply => {
            RuntimeValue::Integer(lhs.eval(Rc::clone(&ctx))?.int()? * rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::Divide => {
            RuntimeValue::Integer(lhs.eval(Rc::clone(&ctx))?.int()? / rhs.eval(ctx)?.int()?)
        }
        BinaryOperator::Concatenate => RuntimeValue::String(format!(
            "{}{}",
            lhs.eval(Rc::clone(&ctx))?.string()?,
            rhs.eval(ctx)?.string()?
        )),
    })
}

fn do_unary_operation(
    op: UnaryOperator,
    erand: &Expression,
    ctx: Rc<Context>,
) -> Result<RuntimeValue, Error> {
    match op {
        UnaryOperator::Negate => Ok(RuntimeValue::Integer(-erand.eval(ctx)?.int()?)),
    }
}

fn do_function_call(fun: RuntimeValue, args: Vec<RuntimeValue>) -> Result<RuntimeValue, Error> {
    match fun {
        RuntimeValue::Function(f) => {
            match f.borrow() {
                FunctionType::Lambda(Lambda {
                    parent_scope,
                    parameters,
                    body,
                }) => {
                    let scope = Context::new(Rc::clone(parent_scope));
                    if parameters.len() != args.len() {
                        Err(RuntimeError::ExpectedArgumentsFound(
                            parameters.len(),
                            args.len(),
                        ))?
                    }
                    // Bind arguments to parameter names
                    for (ident, val) in parameters.iter().zip(args.into_iter()) {
                        scope.declare(ident.name.to_owned(), val, false);
                    }

                    body.eval(Rc::new(scope))
                }
                FunctionType::Intrinsic(f) => f.call(args),
            }
        }
        x => Err(RuntimeError::ExpectedButFound(RuntimeType::Function, x.get_type()).into()),
    }
}

impl Eval for Block {
    fn eval(&self, parent: Rc<Context>) -> Result<RuntimeValue, Error> {
        let ctx = Rc::new(Context::new(parent));
        self.eval_with_context(ctx)
    }
}
impl Block {
    fn eval_with_context(&self, ctx: Rc<Context>) -> Result<RuntimeValue, Error> {
        let Self(nodes) = self;
        let mut ret = RuntimeValue::None;
        for node in nodes.iter() {
            ret = node.eval(Rc::clone(&ctx))?;
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
