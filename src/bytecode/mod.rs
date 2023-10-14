pub(self) mod instruction;
pub mod interpreter;
pub(self) mod value;

pub fn run_main() {
    use self::instruction::Instruction;
    use self::instruction::Source;
    use crate::ast::BinaryOperator;
    use crate::bytecode::interpreter::BytecodeInterpreter;
    use crate::bytecode::value::Value;
    use crate::interpreter::Interpreter;

    let program = vec![
        Instruction::CreateScope { locals: 1 },
        Instruction::Binary {
            op: BinaryOperator::Add,
            src0: Source::Immediate(Value::Integer(5)),
            src1: Source::Immediate(Value::Integer(3)),
        },
        Instruction::Store {
            dest: Source::Local(0),
        },
        Instruction::Binary {
            op: BinaryOperator::GreaterThan,
            src0: Source::Local(0),
            src1: Source::Immediate(Value::Integer(0)),
        },
        Instruction::JumpFalse { jump_dest: 8 },
        Instruction::Binary {
            op: BinaryOperator::Subtract,
            src0: Source::Local(0),
            src1: Source::Immediate(Value::Integer(1)),
        },
        Instruction::Store {
            dest: Source::Local(0),
        },
        Instruction::UnconditionalJump { jump_dest: 3 },
        Instruction::DestroyScope { locals: 1 },
        Instruction::Noop,
    ];

    println!(
        "{}",
        BytecodeInterpreter::new()
            .interpret_bytecode(program)
            .unwrap()
    )
}
