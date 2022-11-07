use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

pub type Construct = crate::error::AstConstruct;

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
    // Expression(Expression),
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
    While(Expression, Block),
    IfElse(Expression, Block, Block),
}

/*
Expression
: lambda (Args) { body }
| Term + Expression
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
;
*/
#[derive(Debug, Clone)]
pub enum Factor {
    Literal(Literal),
    Expression(Box<Expression>),
    Negate(Box<Factor>),
    Variable(Identifier),
    Lambda(Vec<Identifier>, Block),
    FunctionCall(Identifier, Vec<Expression>), // TODO: hold Expression not identifier ( will change parsing ':(' )
}

#[derive(Debug, Clone)]
pub enum Lvalue {
    Identifier(Identifier),
}
impl Lvalue {
    pub fn name(&self) -> Option<&String> {
        match self {
            Lvalue::Identifier(ident) => Some(&ident.name),
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
            // AstNode::Expression(e) => e,
            AstNode::Statement(s) => write!(f, "{:width$}", s, width = w + INDENT_INCREASE),
            // AstNode::Block(b) => write!(f, "{:width$}", b, width = w + INDENT_INCREASE),
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
            Statement::Declaration(i, e) => {
                writeln!(f, "{:w$}Assignment:", "")?;
                write!(f, "{:width$}", i, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Statement::Assignment(l, e) => {
                writeln!(f, "{:w$}Reassignment:", "")?;
                write!(f, "{:width$}", l, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Statement::While(cond, body) => {
                writeln!(f, "{:w$}While:", "")?;
                write!(f, "{:width$}", cond, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
            }
            Statement::IfElse(cond, body, else_) => {
                writeln!(f, "{:w$}IfElse:", "")?;
                write!(f, "{:width$}", cond, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", else_, width = w + INDENT_INCREASE)?;
            }
            Statement::Expression(e) => {
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
            Expression::Add(t, e) => {
                writeln!(f, "{:w$}Add:", "")?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Expression::Subtract(t, e) => {
                writeln!(f, "{:w$}Subtract:", "")?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Expression::Term(term) => {
                write!(f, "{:w$}", term)?;
            }
            Expression::Compare(t, e, op) => {
                writeln!(f, "{:w$}Comparison ({:?}):", "", op)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
        }
        Ok(())
    }
}
impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Term::Multiply(fac, t) => {
                writeln!(f, "{:w$}Multiply:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Term::Divide(fac, t) => {
                writeln!(f, "{:w$}Divide:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Term::Concatenate(fac, t) => {
                writeln!(f, "{:w$}Concatenate:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
                write!(f, "{:width$}", t, width = w + INDENT_INCREASE)?;
            }
            Term::Factor(fac) => {
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
            Factor::Literal(literal) => {
                // writeln!(f, "{:w$}", literal)?;
                writeln!(f, "{:w$}Literal:", "")?;
                write!(f, "{:width$}", literal, width = w + INDENT_INCREASE)?;
            }
            Factor::Expression(e) => {
                writeln!(f, "{:w$}Expression:", "")?;
                write!(f, "{:width$}", e, width = w + INDENT_INCREASE)?;
            }
            Factor::Negate(fac) => {
                writeln!(f, "{:w$}Negate:", "")?;
                write!(f, "{:width$}", fac, width = w + INDENT_INCREASE)?;
            }
            Factor::Variable(i) => {
                write!(f, "{:w$}", i)?;
            }
            Factor::FunctionCall(fun, args) => {
                writeln!(f, "{:w$}FunctionCall:", "")?;
                write!(f, "{:width$}", fun, width = w + INDENT_INCREASE)?;
                writeln!(f, "{:width$}Args:", "", width = w + INDENT_INCREASE)?;
                for arg in args {
                    write!(f, "{:width$}", arg, width = w + 2 * INDENT_INCREASE)?;
                }
            }
            Factor::Lambda(args, body) => {
                writeln!(f, "{:w$}Lambda:", "")?;
                for arg in args {
                    write!(f, "{:width$}", arg, width = w + INDENT_INCREASE)?;
                }
                write!(f, "{:width$}", body, width = w + INDENT_INCREASE)?;
            }
        }
        Ok(())
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        // match self {
        //     Literal::IntLiteral(n) => writeln!(f, "{:w$}IntLiteral({})", "", n)?,
        //     Literal::StringLiteral(n) => writeln!(f, "{:w$}StringLiteral({})", "", n)?,
        //     Literal::BooleanLiteral(n) => writeln!(f, "{:w$}BooleanLiteral({})", "", n)?,
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
            Lvalue::Identifier(n) => writeln!(f, "{:w$}", n)?,
        }
        Ok(())
    }
}
