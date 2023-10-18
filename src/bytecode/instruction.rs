use std::rc::Rc;

use crate::ast::{BinaryOperator, UnaryOperator};

use super::value::Value;

#[derive(Debug, Clone)]
pub enum Source {
    Immediate(Value),
    Result,
    Stack,
    Local(usize),
    Global(Rc<str>),
}

#[derive(Debug)]
pub enum Instruction {
    // Load src to Result
    Nullary {
        src: Source,
    },
    Binary {
        op: BinaryOperator,
        src0: Source,
        src1: Source,
    },
    Unary {
        op: UnaryOperator,
        src0: Source,
    },
    JumpTrue {
        jump_dest: usize,
    },
    JumpFalse {
        jump_dest: usize,
    },
    UnconditionalJump {
        jump_dest: usize,
    },
    // With, // TODO
    // New,  // TODO
    CreateScope {
        locals: usize,
    },
    DestroyScope {
        locals: usize,
    },
    // Store Result to dest
    Store {
        dest: Source,
    },
    Call {
        argc: usize,
    },
    Noop,
}
