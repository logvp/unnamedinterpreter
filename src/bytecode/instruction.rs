use crate::ast::{BinaryOperator, UnaryOperator};

use super::value::Value;

#[derive(Debug)]
pub enum Source {
    Immediate(Value),
    Result,
    Temporary(usize),
    Local(usize),
    Global(String),
}

#[derive(Debug)]
pub enum Instruction {
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
    // Call, // TODO
    CreateScope {
        locals: usize,
    },
    DestroyScope {
        locals: usize,
    },
    Noop,
    Store {
        dest: Source,
    },
}
