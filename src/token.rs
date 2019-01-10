use std::fmt;

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
    Invalid(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // Identifiers + literals
            Token::Ident(ident) => write!(f, "{}", ident),
            Token::Int(val) => write!(f, "{}", val),
            Token::String(val) => write!(f, "{}", val),
            Token::Bool(val) => write!(f, "{}", val),

            // Statements
            Token::Assign => write!(f, "="),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),

            // Operators
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Bang => write!(f, "!"),
            Token::Percent => write!(f, "%"),

            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::LessThanEqual => write!(f, "<="),
            Token::GreaterThan => write!(f, ">"),
            Token::GreaterThanEqual => write!(f, ">="),
            Token::AndAnd => write!(f, "&&"),
            Token::OrOr => write!(f, "||"),

            // Delimiters
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Or => write!(f, "|"),

            // Reseved keywords
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),

            // Control tokens
            Token::Eof => write!(f, ""),
            Token::Invalid(t) => write!(f, "{}", t),
        }
    }
}
