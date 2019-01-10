use crate::Object;
use std::{error, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    RuntimeError(String),
    ParseError(String),
    ReturnObject(Object), // 評価器内で戻り値の追跡に利用する
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::RuntimeError(msg) => write!(f, "runtime error: {}", msg),
            Error::ParseError(msg) => write!(f, "parse error: {}", msg),
            Error::ReturnObject(_) => panic!("cannot format Error::ReturnObject"),
        }
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
