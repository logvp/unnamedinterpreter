use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

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

#[derive(Debug)]
pub enum AstNode {
    // Expression(Expression),
    Statement(Statement),
}

/*
Statement
: let Identifier = Expression ;
: set Lvalue = Expression ;
: Identifier := Expression ;
: Lvalue = Expression ;
| Expression ;
;
*/
#[derive(Debug)]
pub enum Statement {
    Declaration(Identifier, Expression),
    Assignment(Lvalue, Expression),
    Expression(Expression),
}

/*
Expression
: Term + Expression
| Term - Expression
| Term
;
*/
#[derive(Debug)]
pub enum Expression {
    Add(Term, Box<Expression>),
    Subtract(Term, Box<Expression>),
    Term(Term),
}

/*
Term
: Factor * Term
| Factor / Term
| Factor
;
*/
#[derive(Debug)]
pub enum Term {
    Multiply(Factor, Box<Term>),
    Divide(Factor, Box<Term>),
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
#[derive(Debug)]
pub enum Factor {
    Literal(Literal),
    Expression(Box<Expression>),
    Negate(Box<Factor>),
    Variable(Identifier),
    Function(Identifier, Vec<Expression>), // hold Expression not identifier ( will change parsing ':(' )
}

#[derive(Debug)]
pub enum Lvalue {
    Identifier(Identifier),
}
impl Lvalue {
    pub fn name(&self) -> &String {
        match self {
            Lvalue::Identifier(ident) => &ident.name,
            _ => panic!("name is only supported for identifiers"),
        }
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum Literal {
    IntLiteral(i32),
    StringLiteral(String),
}

/*
pub enum Token {
    Literal(Literal),
    LeftParen,
    RightParen,
    LeftAngle,
    RightAngle,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    Eof,
}
 */

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
        write!(
            f,
            "{:width$}",
            match self {
                // AstNode::Expression(e) => e,
                AstNode::Statement(s) => s,
            },
            width = w + INDENT_INCREASE
        )
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
            Factor::Function(fun, args) => {
                let mut first = true;
                write!(f, "{:w$}(", fun)?;
                for arg in args {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                    first = false;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = if let Some(n) = f.width() { n } else { 0 };
        match self {
            Literal::IntLiteral(n) => writeln!(f, "{:w$}IntLiteral({})", "", n)?,
            Literal::StringLiteral(n) => writeln!(f, "{:w$}StringLiteral({})", "", n)?,
        }
        Ok(())
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
