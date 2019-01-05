use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
}

impl Object {
    pub fn get_type_name(&self) -> &str {
        match self {
            Object::Int(_) => "Int",
            Object::String(_) => "String",
            Object::Bool(_) => "Bool",
            Object::Unit => "Unit",
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "\"{}\"", val),
            Object::Bool(val) => write!(f, "{}", val),
            Object::Unit => write!(f, "()"),
        }
    }
}
