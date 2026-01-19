pub mod environment;

use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(f64),
    String(String),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Builtin(BuiltinFunction),
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
        }
    }
}

pub type BuiltinFunction = fn(Vec<Object>) -> Object;
