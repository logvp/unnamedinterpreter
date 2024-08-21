use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    ast::{Block, Identifier},
    error::RuntimeError,
    interpreter::RuntimeType,
};

use super::instrinsics::IntrinsicFunction;

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Object(Object),
    Function(Rc<FunctionType>),
    Integer(i32),
    String(crate::String),
    Boolean(bool),
    None,
}
impl Default for RuntimeValue {
    fn default() -> Self {
        Self::None
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
    pub(super) fn int(&self) -> Result<i32, RuntimeError> {
        if let Self::Integer(int) = self {
            Ok(*int)
        } else {
            Err(RuntimeError::ExpectedButFound(
                RuntimeType::Integer,
                self.get_type(),
            ))
        }
    }

    pub(super) fn string(&self) -> Result<crate::String, RuntimeError> {
        if let Self::String(string) = self {
            Ok(string.clone())
        } else {
            Err(RuntimeError::ExpectedButFound(
                RuntimeType::String,
                self.get_type(),
            ))
        }
    }

    pub(super) fn boolean(&self) -> Result<bool, RuntimeError> {
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
pub struct Object(pub(super) Rc<Context>);

#[derive(Debug)]
pub struct Lambda {
    pub(super) parent_scope: Rc<Context>,
    pub(super) parameters: Rc<[Identifier]>,
    pub(super) body: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub(super) struct Variable {
    pub(super) val: RuntimeValue,
    is_const: bool,
}

#[derive(Clone)]
pub(super) struct Context {
    data: RefCell<HashMap<crate::String, Variable>>,
    parent: Option<Rc<Self>>,
}
impl Context {
    pub(super) fn init_global() -> Self {
        let global = Context {
            data: Default::default(),
            parent: None,
        };
        global.declare(
            "print".into(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::Print))),
            true,
        );
        global.declare(
            "typeof".into(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::TypeOf))),
            true,
        );
        global.declare(
            "debug".into(),
            RuntimeValue::Function(Rc::new(FunctionType::Intrinsic(IntrinsicFunction::Debug))),
            true,
        );

        global
    }

    pub(super) fn is_const(&self, key: &crate::String) -> Option<bool> {
        if self.data.borrow().contains_key(key) {
            Some(self.data.borrow().get(key).unwrap().is_const)
        } else if let Some(parent) = &self.parent {
            parent.is_const(key)
        } else {
            None
        }
    }

    pub(super) fn contains(&self, key: &crate::String) -> bool {
        if self.data.borrow().contains_key(key) {
            true
        } else if let Some(parent) = &self.parent {
            parent.contains(key)
        } else {
            false
        }
    }

    pub(super) fn contains_in_scope(&self, key: &crate::String) -> bool {
        self.data.borrow().contains_key(key)
    }

    pub(super) fn declare(&self, key: crate::String, val: RuntimeValue, is_const: bool) {
        let None = self
            .data
            .borrow_mut()
            .insert(key, Variable { val, is_const })
        else {
            panic!("Cannot redeclare variable in same scope")
        };
    }

    pub(super) fn update(&self, key: crate::String, val: RuntimeValue) -> Result<(), RuntimeError> {
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

    pub(super) fn get(&self, key: &crate::String) -> Option<Variable> {
        self.data
            .borrow()
            .get(key)
            .cloned()
            .or(self.parent.as_ref().and_then(|p| p.get(key)))
    }

    pub(super) fn new(parent: Rc<Self>) -> Self {
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
