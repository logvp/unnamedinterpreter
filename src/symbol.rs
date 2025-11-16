use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::Deref;
use std::sync::{Mutex, OnceLock};

static INTERNER: OnceLock<Mutex<HashMap<InternerKey, &str>>> = OnceLock::new();

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct InternerKey {
    key: u64,
}
impl InternerKey {
    fn get(string: &str) -> Self {
        let mut h = DefaultHasher::default();
        string.hash(&mut h);
        InternerKey { key: h.finish() }
    }
}

fn intern(string: &str) -> InternerKey {
    let mut interner = INTERNER.get_or_init(Default::default).lock().unwrap();
    let key = InternerKey::get(string);
    interner
        .entry(key)
        .or_insert_with(|| String::from(string).leak());
    key
}

pub fn sym(string: &str) -> Symbol {
    Symbol::new(string)
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    key: InternerKey,
}
impl Symbol {
    fn new(string: &str) -> Self {
        Self {
            key: intern(string),
        }
    }

    pub fn get_str(self) -> &'static str {
        let interner = INTERNER.get().unwrap().lock().unwrap();
        interner.get(&self.key).unwrap()
    }
}
impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.get_str(), f)
    }
}
impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.get_str(), f)
    }
}
impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.get_str()
    }
}
