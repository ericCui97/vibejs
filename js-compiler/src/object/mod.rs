pub mod environment;

use std::fmt;

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(f64),
    String(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Builtin(BuiltinFunction),
    Hash(HashMap<ObjectKey, Object>),
}

// We need a key type that implements Eq + Hash for HashMap
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ObjectKey {
    Integer(i64), // Use i64 for hashing f64 (lossy but works for simple cases)
    String(String),
    Boolean(bool),
}

impl ObjectKey {
    pub fn from_object(obj: &Object) -> Option<ObjectKey> {
        match obj {
            Object::Integer(val) => Some(ObjectKey::Integer(*val as i64)),
            Object::String(val) => Some(ObjectKey::String(val.clone())),
            Object::Boolean(val) => Some(ObjectKey::Boolean(*val)),
            _ => None,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Hash(pairs) => {
                let mut strings = vec![];
                for (key, value) in pairs {
                    strings.push(format!("{}: {}", key, value));
                }
                write!(f, "{{{}}}", strings.join(", "))
            },
        }
    }
}

impl fmt::Display for ObjectKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectKey::Integer(val) => write!(f, "{}", val),
            ObjectKey::String(val) => write!(f, "{}", val),
            ObjectKey::Boolean(val) => write!(f, "{}", val),
        }
    }
}

pub type BuiltinFunction = fn(Vec<Object>) -> Object;
