use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[derive(Debug, Clone, Copy)]
pub enum Construct {
    TopLevel,
    BlockScope,
    Statement,
    Var,
    Let,
    Set,
    Expression,
    Lvalue,
    FunctionCall,
    ParameterList,
    IfCondition,
    IfBody,
    ElseBody,
    WhileCondition,
    WhileBody,
    With,
    NewBlock,
    Lambda,
    LambdaBody,
    SimpleAssignment,
    SimpleDeclaration,
}

#[derive(Debug, Default)]
pub struct Ast {
    pub nodes: Vec<AstNode>,
}
impl Deref for Ast {
    type Target = Vec<AstNode>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}
impl DerefMut for Ast {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Expression(Expression),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct Block(pub Rc<[AstNode]>);
thread_local! {
    static EMPTY_BLOCK_DATA: Rc<[AstNode]> = Rc::new([]);
}
impl Block {
    pub fn empty() -> Self {
        Block(EMPTY_BLOCK_DATA.with(|x| x.clone()))
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Identifier, Expression, bool),
    Assignment(Lvalue, Expression),
    Expression(Expression),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    // Comparisons
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    // String
    Concatenate,
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOperator {
    Negate,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(PrefixOperator, Box<Expression>),
    IfElse(Box<Expression>, Block, Block),
    While(Box<Expression>, Block),
    With(Box<Expression>, Block),
    New(Block),
    Lambda(Rc<[Identifier]>, Block),
    FunctionCall(Box<Expression>, Rc<[Expression]>),
    Block(Block),
    Literal(Literal),
    Variable(Identifier),
}

#[derive(Debug, Clone)]
pub enum Lvalue {
    Identifier(Identifier),
}
impl Lvalue {
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Identifier(ident) => Some(&ident.name),
            // _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i32),
    String(String),
    Boolean(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self)
    }
}

impl Display for Construct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::TopLevel => "Top Level Program",
                Self::BlockScope => "Scoped Block",
                Self::Statement => "Statement",
                Self::Let => "Let Declaration",
                Self::Var => "Var Declaration",
                Self::Set => "Set Statement",
                Self::Expression => "Expression",
                Self::Lvalue => "Lvalue",
                Self::FunctionCall => "Function Call",
                Self::ParameterList => "Parameter List",
                Self::IfCondition => "If Expression Condition",
                Self::IfBody => "If Expression Body",
                Self::ElseBody => "Else Expression Body",
                Self::WhileCondition => "While Expression Condition",
                Self::WhileBody => "While Expression Body",
                Self::With => "With Expression",
                Self::NewBlock => "New Expression",
                Self::Lambda => "Lambda Expression",
                Self::LambdaBody => "Lambda Expression Body",
                Self::SimpleAssignment => "Assignment",
                Self::SimpleDeclaration => "Declaration",
            }
        )
    }
}
