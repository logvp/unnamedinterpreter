use std::{borrow::Borrow, fmt::Display, rc::Rc};

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
impl Borrow<str> for String {
    fn borrow(&self) -> &str {
        &self.0
    }
}
thread_local! {
    pub static EMPTY: crate::String = crate::String::from("");
}
impl String {
    pub fn empty() -> Self {
        EMPTY.with(Clone::clone)
    }
}
