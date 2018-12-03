#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Identifiers + literals
    Ident(String),
    Int(i64),
    String(String),

    // Statements
    Assign,

    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Percent,

    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    // Delimiters
    Semicolon,
    Lparen,
    Rparen,

    // Reseved keywords
    Let,

    // Control tokens
    Eof,
    Invalid,
}
