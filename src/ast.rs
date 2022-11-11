use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub enum ConstructKind {
    Ast,
    AstNode,
    Block,
    Statement,
    Let,
    Set,
    Expression,
    Term,
    Factor,
    Lvalue,
    Identifier,
    Literal,
    FunctionCall,
    ParameterList,
    Lambda,
}

#[derive(Debug, Default)]
pub struct Ast {
    nodes: Vec<AstNode>,
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
    // Block(Block),
}

#[derive(Debug, Clone, Default)]
pub struct Block(pub Vec<AstNode>);

/*
Statement
: let Identifier = Expression ;
: set Lvalue = Expression ;
: Identifier := Expression ;
: Lvalue = Expression ;
| Expression ;
;
*/
#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Identifier, Expression),
    Assignment(Lvalue, Expression),
    Expression(Expression),
}

/*
Expression
: Term + Expression
| Term - Expression
| Term Comparison Expression
| Term
;
*/
#[derive(Debug, Clone)]
pub enum Expression {
    Add(Term, Box<Expression>),
    Subtract(Term, Box<Expression>),
    Compare(Term, Box<Expression>, Comparison),
    IfElse(Box<Expression>, Block, Block),
    While(Box<Expression>, Block),
    Term(Term),
}
#[derive(Debug, Clone)]
pub enum Comparison {
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

/*
Term
: Factor * Term
| Factor / Term
| Factor ++ Term
| Factor
;
*/
#[derive(Debug, Clone)]
pub enum Term {
    Multiply(Factor, Box<Term>),
    Divide(Factor, Box<Term>),
    Concatenate(Factor, Box<Term>),
    Factor(Factor),
}

/*
Factor
: Literal
| Identifier
| ( Expression )
| - Factor
| lambda ( Identifier* ) Block
| Factor ( Expression* )
;
*/
#[derive(Debug, Clone)]
pub enum Factor {
    Literal(Literal),
    Expression(Box<Expression>),
    Negate(Box<Factor>),
    Variable(Identifier),
    Lambda(Vec<Identifier>, Block),
    FunctionCall(Box<Factor>, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum Lvalue {
    Identifier(Identifier),
}
impl Lvalue {
    pub fn name(&self) -> Option<&String> {
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
    IntLiteral(i32),
    StringLiteral(String),
    BooleanLiteral(bool),
}

const INDENT_INCREASE: usize = 2;
impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        for node in self.nodes.iter() {
            write!(f, "{:w$}", node)?
        }
        Ok(())
    }
}
impl Display for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        writeln!(f, "{:w$}AstNode:", "")?;
        match self {
            Self::Expression(e) => write!(f, "{:width$}", e, width = w + INDENT_INCREASE),
            Self::Statement(s) => write!(f, "{:width$}", s, width = w + INDENT_INCREASE),
            // Self::Block(b) => write!(f, "{:width$}", b, width = w + INDENT_INCREASE),
        }
    }
}
impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        writeln!(f, "{:w$}Block:", "")?;
        for line in self.0.iter() {
            write!(f, "{:width$}", line, width = w + INDENT_INCREASE)?
        }
        Ok(())
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Self::Declaration(i, e) => {
                writeln!(f, "{:w$}Assignment:", "")?;
                write!(f, "{:width$}", i, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::Assignment(l, e) => {
                writeln!(f, "{:w$}Reassignment:", "")?;
                write!(f, "{:width$}", l, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::Expression(e) => {
                write!(f, "{:w$}", e)?;
            }
        }
        Ok(())
    }
}
impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Self::Add(t, e) => {
                writeln!(f, "{:w$}Add:", "")?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::Subtract(t, e) => {
                writeln!(f, "{:w$}Subtract:", "")?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::Term(term) => {
                write!(f, "{:w$}", term)?;
            }
            Self::Compare(t, e, op) => {
                writeln!(f, "{:w$}Comparison ({:?}):", "", op)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::IfElse(cond, body, else_) => {
                writeln!(f, "{:w$}IfElse:", "")?;
                write!(f, "{:width$}", cond, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", else_, width = w + INDENT_INCREASE)?;
            }
            Self::While(cond, body) => {
                writeln!(f, "{:w$}While:", "")?;
                write!(f, "{:width$}", cond, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
            }
        }
        Ok(())
    }
}
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Self::Multiply(fac, t) => {
                writeln!(f, "{:w$}Multiply:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Self::Divide(fac, t) => {
                writeln!(f, "{:w$}Divide:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Self::Concatenate(fac, t) => {
                writeln!(f, "{:w$}Concatenate:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Self::Factor(fac) => {
                write!(f, "{:w$}", fac)?;
            }
        }
        Ok(())
    }
}
impl Display for Factor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Self::Literal(literal) => {
                // writeln!(f, "{:w$}", literal)?;
                writeln!(f, "{:w$}Literal:", "")?;
                write!(f, "{:width$}", literal, width = w + INDENT_INCREASE)?;
            }
            Self::Expression(e) => {
                writeln!(f, "{:w$}Expression:", "")?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Self::Negate(fac) => {
                writeln!(f, "{:w$}Negate:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
            }
            Self::Variable(i) => {
                write!(f, "{:w$}", i)?;
            }
            Self::Lambda(args, body) => {
                writeln!(f, "{:w$}Lambda:", "")?;
                for arg in args {
                    write!(f, "{:width$}", arg, width = w + INDENT_INCREASE)?;
                }
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
            }
            Self::FunctionCall(fun, args) => {
                writeln!(f, "{:w$}FunctionCall:", "")?;
                write!(f, "{:width$}", fun, width = w + INDENT_INCREASE)?;
                writeln!(f, "{:width$}Args:", "", width = w + INDENT_INCREASE)?;
                for arg in args {
                    write!(f, "{:width$}", arg, width = w + 2 * INDENT_INCREASE)?;
                }
            }
        }
        Ok(())
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        // match self {
        //     Self::IntLiteral(n) => writeln!(f, "{:w$}IntLiteral({})", "", n)?,
        //     Self::StringLiteral(n) => writeln!(f, "{:w$}StringLiteral({})", "", n)?,
        //     Self::BooleanLiteral(n) => writeln!(f, "{:w$}BooleanLiteral({})", "", n)?,
        // }
        // Ok(())
        writeln!(f, "{:w$}{:?}", "", self)
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        writeln!(f, "{:w$}Identifier(\"{}\")", "", self.name)
    }
}
impl Display for Lvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Self::Identifier(n) => writeln!(f, "{:w$}", n)?,
        }
        Ok(())
    }
}
impl Display for ConstructKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ast => "Ast",
                Self::AstNode => "Ast Node",
                Self::Block => "Block",
                Self::Statement => "Statement",
                Self::Let => "Let",
                Self::Set => "Set",
                Self::Expression => "Expression",
                Self::Term => "Term",
                Self::Factor => "Factor",
                Self::Lvalue => "Lvalue",
                Self::Identifier => "Identifier",
                Self::Literal => "Literal",
                Self::FunctionCall => "Function Call",
                Self::ParameterList => "Parameter List",
                Self::Lambda => "Lambda Expression",
            }
        )
    }
}
