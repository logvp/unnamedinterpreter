use std::{fmt::Display, rc::Rc};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct String(Rc<str>);
impl Display for String {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> From<T> for String
where
    T: Into<Rc<str>>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
impl AsRef<str> for String {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

thread_local! {
    pub static EMPTY: String = String::from("");
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Symbol(String);
impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<T> From<T> for Symbol
where
    T: Into<String>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Symbol {
    pub fn dont_care() -> Self {
        Self(EMPTY.with(Clone::clone))
    }
}
