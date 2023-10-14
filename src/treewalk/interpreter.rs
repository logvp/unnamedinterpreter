use std::borrow::Borrow;
use std::rc::Rc;

use crate::ast::*;
use crate::error::{Error, RuntimeError};
use crate::interpreter::{Interpreter, RuntimeType};
use crate::parser::Parser;

use super::runtime::{Context, FunctionType, Lambda, Object, Variable};
use super::RuntimeValue;

pub struct TreeWalkInterpreter {
    context: Rc<Context>,
}
impl Interpreter for TreeWalkInterpreter {
    type ReplReturn = RuntimeValue;

    fn new() -> Self {
        TreeWalkInterpreter {
            context: Rc::new(Context::init_global()),
        }
    }

    fn interpret(
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
impl TreeWalkInterpreter {
    fn interpret_node(&mut self, node: &AstNode) -> Result<RuntimeValue, Error> {
        node.eval(Rc::clone(&self.context))
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
