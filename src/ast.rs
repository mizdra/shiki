/// shiki 言語の AST.
use std::fmt;

/// 識別子を表す Node
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Ident(pub String);

impl Ident {
    pub fn get_ident_name(&self) -> String {
        self.0.clone()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// 前置演算子を表す Node
#[derive(PartialEq, Clone, Debug)]
pub enum Prefix {
    Plus,
    Minus,
    Not,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Prefix::Plus => write!(f, "+"),
            Prefix::Minus => write!(f, "-"),
            Prefix::Not => write!(f, "!"),
        }
    }
}

/// 中置演算子を表す Node
#[derive(PartialEq, Clone, Debug)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    GreaterThan,
    LessThanEqual,
    LessThan,
    AndAnd,
    OrOr,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Divide => write!(f, "/"),
            Infix::Multiply => write!(f, "*"),
            Infix::Equal => write!(f, "=="),
            Infix::NotEqual => write!(f, "!="),
            Infix::GreaterThanEqual => write!(f, ">="),
            Infix::GreaterThan => write!(f, ">"),
            Infix::LessThanEqual => write!(f, "<="),
            Infix::LessThan => write!(f, "<"),
            Infix::AndAnd => write!(f, "&&"),
            Infix::OrOr => write!(f, "||"),
        }
    }
}

/// 式を表す Node
#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expr>),
    Infix(Infix, Box<Expr>, Box<Expr>),
    Block(BlockStmt),
    If(Box<Expr>, BlockStmt, Option<BlockStmt>),
    Lambda(Vec<Ident>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    While(Box<Expr>, BlockStmt),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Ident(ident) => write!(f, "{}", ident),
            Expr::Literal(literal) => write!(f, "{}", literal),
            Expr::Prefix(op, expr) => write!(f, "({}{})", op, expr),
            Expr::Infix(op, left, right) => write!(f, "({} {} {})", left, op, right),
            Expr::If(cond, _consequence, alternative) => match alternative {
                Some(_block_stmt) => write!(f, "if {} {{ ... }} ...", cond),
                None => write!(f, "if {} {{ ... }}", cond),
            },
            _ => write!(f, "UNIMPLEMENTED"),
        }
    }
}

/// リテラルを表す Node
#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
    Unit,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Literal::Int(val) => write!(f, "{}", val),
            Literal::String(val) => write!(f, "\"{}\"", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::Unit => write!(f, "()"),
        }
    }
}

/// 文を表す Node
#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Assign(Ident, Expr),
    Return(Expr),
    Expr(Expr),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Stmt::Assign(ident, expr) => write!(f, "{} = {};", ident, expr),
            Stmt::Return(expr) => write!(f, "return {};", expr),
            Stmt::Expr(expr) => write!(f, "{};", expr),
        }
    }
}

/// 複文を表す Node
pub type BlockStmt = Vec<Stmt>;

/// プログラムを表す Node
pub type Program = Vec<Stmt>;

/// 演算子の優先順位
#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    AndAnd,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // call_lambda(x)
}
