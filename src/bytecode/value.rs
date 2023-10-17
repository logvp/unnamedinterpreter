use std::fmt::Display;

use crate::{
    ast::{BinaryOperator, UnaryOperator},
    error::RuntimeError,
    interpreter::RuntimeType,
    lexer::Literal,
};

use super::intrinsics::IntrinsicFunction;

#[derive(Clone, Debug)]
pub enum FunctionObject {
    Lambda { arity: usize, code: usize },
    Intrinsic(IntrinsicFunction),
}

#[derive(Clone, Debug)]
pub enum Value {
    // Object(), TODO
    Function(FunctionObject),
    Integer(i32),
    String(String),
    Boolean(bool),
    None,
}
impl Value {
    pub fn type_of(&self) -> RuntimeType {
        match self {
            Value::Function { .. } => RuntimeType::Function,
            Value::Integer(_) => RuntimeType::Integer,
            Value::Boolean(_) => RuntimeType::Boolean,
            Value::String(_) => RuntimeType::String,
            Value::None => RuntimeType::None,
        }
    }

    pub fn boolean(&self) -> Result<bool, RuntimeError> {
        match self {
            Value::Boolean(x) => Ok(*x),
            x => Err(RuntimeError::ExpectedButFound(
                RuntimeType::Boolean,
                x.type_of(),
            )),
        }
    }

    pub fn integer(&self) -> Result<i32, RuntimeError> {
        match self {
            Value::Integer(x) => Ok(*x),
            x => Err(RuntimeError::ExpectedButFound(
                RuntimeType::Integer,
                x.type_of(),
            )),
        }
    }

    pub fn string(&self) -> Result<String, RuntimeError> {
        match self {
            Value::String(x) => Ok(x.clone()),
            x => Err(RuntimeError::ExpectedButFound(
                RuntimeType::String,
                x.type_of(),
            )),
        }
    }

    pub fn function(&self) -> Result<FunctionObject, RuntimeError> {
        match self {
            Value::Function(x) => Ok(x.clone()),
            x => Err(RuntimeError::ExpectedButFound(
                RuntimeType::Function,
                x.type_of(),
            )),
        }
    }

    pub fn binary_operation(op: BinaryOperator, a: Value, b: Value) -> Result<Value, RuntimeError> {
        use BinaryOperator as Op;
        Ok(match op {
            Op::Equal => Value::Boolean(a == b),
            Op::NotEqual => Value::Boolean(a != b),
            Op::LessThan => Value::Boolean(a.integer()? < b.integer()?),
            Op::LessEqual => Value::Boolean(a.integer()? <= b.integer()?),
            Op::GreaterThan => Value::Boolean(a.integer()? > b.integer()?),
            Op::GreaterEqual => Value::Boolean(a.integer()? >= b.integer()?),
            Op::Add => Value::Integer(a.integer()? + b.integer()?),
            Op::Subtract => Value::Integer(a.integer()? - b.integer()?),
            Op::Multiply => Value::Integer(a.integer()? * b.integer()?),
            Op::Divide => Value::Integer(a.integer()? / b.integer()?),
            Op::Concatenate => Value::String(format!("{}{}", a.string()?, b.string()?)),
        })
    }

    pub fn unary_operation(op: UnaryOperator, a: Value) -> Result<Value, RuntimeError> {
        use UnaryOperator as Op;
        Ok(match op {
            Op::Negate => Value::Integer(-a.integer()?),
        })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function(FunctionObject::Lambda { arity, code }) => {
                write!(f, "<lambda({}){{{}}}>", arity, code)
            }
            Self::Function(FunctionObject::Intrinsic(id)) => {
                write!(f, "<intrinsic {:?}>", id) // TODO implement display for Intrinsic
            }
            Self::Integer(int) => write!(f, "{}", int),
            Self::String(string) => write!(f, "{}", string),
            Self::Boolean(boolean) => write!(f, "{}", boolean),
            Self::None => write!(f, "NoneType"),
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::None, Self::None) => true,
            _ => false,
        }
    }
}
impl Default for Value {
    fn default() -> Self {
        Value::None
    }
}
impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Boolean(x) => Value::Boolean(x),
            Literal::Integer(x) => Value::Integer(x),
            Literal::String(x) => Value::String(x),
        }
    }
}
