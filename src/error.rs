use std::{error, fmt};

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    RuntimeError(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::RuntimeError(msg) => write!(f, "runtime error: {}", msg),
        }
    }
}

impl error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;
