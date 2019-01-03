#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Identifiers + literals
    Ident(String),
    Int(i64),
    String(String),
    Bool(bool),

    // Statements
    Assign,
    If,
    Else,
    While,

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
    AndAnd,
    OrOr,

    // Delimiters
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Or,

    // Reseved keywords
    Let,
    Return,

    // Control tokens
    Eof,
    Invalid,
}
