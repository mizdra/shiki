/// shiki 言語のオブジェクト (式を評価して得られる値).
use crate::ast;
use crate::env::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// 整数オブジェクト
    Int(i64),
    /// 文字列オブジェクト
    String(String),
    /// 真偽値オブジェクト
    Bool(bool),
    /// Unit オブジェクト
    Unit,
    /// Lambda オブジェクト
    Lambda(Rc<RefCell<Env>>, Vec<ast::Ident>, ast::Expr),
}

impl Object {
    /// オブジェクトの型の名前を返します.
    pub fn get_type_name(&self) -> &str {
        match self {
            Object::Int(_) => "Int",
            Object::String(_) => "String",
            Object::Bool(_) => "Bool",
            Object::Unit => "Unit",
            Object::Lambda(..) => "Lambda",
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
            Object::Lambda(..) => write!(f, "|..| {{..}}"),
        }
    }
}
