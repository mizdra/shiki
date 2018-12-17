pub mod ast;

use self::ast::*;
use crate::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    next_token: Token,
}

impl Parser<'_> {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer,
            cur_token: Token::Invalid,
            next_token: Token::Invalid,
        };
        parser.bump();
        parser.bump();
        parser
    }

    fn bump(&mut self) {
        self.cur_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == token
    }

    fn next_token_is(&self, token: Token) -> bool {
        self.next_token == token
    }

    fn token_precedence_is(token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LessThan | Token::LessThanEqual => Precedence::LessGreater,
            Token::GreaterThan | Token::GreaterThanEqual => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::Lparen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn cur_token_precedence(&self) -> Precedence {
        Self::token_precedence_is(&self.cur_token)
    }

    fn next_token_precedence(&self) -> Precedence {
        Self::token_precedence_is(&self.next_token)
    }

    fn skip_semicolons(&mut self) {
        while self.cur_token_is(Token::Semicolon) {
            self.bump();
        }
    }
}

// ident
impl Parser<'_> {
    /// 現在のカーソル位置以降を識別子としてパースし,
    /// 識別子の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_ident(&mut self) -> Option<Ident> {
        // 借用ルールの制約により直接 return するのではなく,
        // 一度変数に格納してから bump し, 返している.
        // Note: NLL が安定化されたら直接 return で返せるようになるはず.
        let ident = match self.cur_token {
            Token::Ident(ref mut ident) => Some(Ident(ident.clone())),
            _ => return None,
        };
        self.bump();
        ident
    }
}

// expr
impl Parser<'_> {
    /// 現在のカーソル位置以降を識別子式としてパースし,
    /// 識別子式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_ident_expr(&mut self) -> Option<Expr> {
        let ident = self.parse_ident()?; // TODO: エラー報告
        Some(Expr::Ident(ident))
    }

    /// 現在のカーソル位置以降を整数リテラル式としてパースし,
    /// 整数リテラル式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_int_expr(&mut self) -> Option<Expr> {
        if let Token::Int(int) = self.cur_token {
            Some(Expr::Literal(Literal::Int(int)))
        } else {
            None // TODO: エラー報告
        }
    }

    /// 現在のカーソル位置以降を文字列式としてパースし,
    /// 文字列式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_string_expr(&mut self) -> Option<Expr> {
        if let Token::String(ref string) = self.cur_token {
            Some(Expr::Literal(Literal::String(string.clone())))
        } else {
            None // TODO: エラー報告
        }
    }

    /// 現在のカーソル位置以降を単項演算子の式としてパースし,
    /// 単項演算子の式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.cur_token {
            Token::Bang => Prefix::Not,
            Token::Plus => Prefix::Plus,
            Token::Minus => Prefix::Minus,
            _ => return None,
        };
        self.bump();
        let expr = self.parse_expr(Precedence::Prefix)?;
        Some(Expr::Prefix(prefix, Box::new(expr)))
    }

    /// 現在のカーソル位置以降を括弧による式としてパースし,
    /// 括弧による式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.bump();
        let expr = self.parse_expr(Precedence::Lowest)?;
        if self.next_token_is(Token::Rparen) {
            self.bump();
            Some(expr)
        } else {
            // TODO: エラー報告
            None
        }
    }

    /// 現在のカーソル位置が中置演算子の式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// 現在のカーソル位置が中置演算子でない場合など,
    /// パースに失敗した場合は None を返します.
    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.cur_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Divide,
            Token::Asterisk => Infix::Multiply,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::LessThan => Infix::LessThan,
            Token::LessThanEqual => Infix::LessThanEqual,
            Token::GreaterThan => Infix::GreaterThan,
            Token::GreaterThanEqual => Infix::GreaterThanEqual,
            // TODO: エラー報告
            _ => return None,
        };

        let precedence = self.cur_token_precedence();

        self.bump();

        match self.parse_expr(precedence) {
            Some(expr) => Some(Expr::Infix(infix, Box::new(left), Box::new(expr))),
            None => None,
        }
    }

    /// 現在のカーソル位置以降を式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    ///
    /// また式はトップダウン構文解析法の1つである
    /// Pratt parserを用いてパースされます.
    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left = match self.cur_token {
            Token::Ident(_) => self.parse_ident_expr()?,
            Token::Int(_) => self.parse_int_expr()?,
            Token::String(_) => self.parse_string_expr()?,
            Token::Bang | Token::Plus | Token::Minus => self.parse_prefix_expr()?,
            Token::Lparen => self.parse_grouped_expr()?,
            _ => {
                // TODO: エラー報告
                return None;
            }
        };

        // 次の演算子/トークン (`next_token`) の左結合力が現在の右結合力よりも高い場合は,
        // これまで構文解析したもの (`left`) は次の演算子に吸い込まれる.
        // 左結合力が現在の右結合力よりも高い状態が続けば,
        // トークンは次の演算子に吸い込まれ続ける.
        while !self.next_token_is(Token::Semicolon) && precedence < self.next_token_precedence() {
            match self.next_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::LessThanEqual
                | Token::GreaterThan
                | Token::GreaterThanEqual => {
                    self.bump();
                    left = self.parse_infix_expr(left)?;
                }
                // 中置演算子としてパースできないので return して呼び出し元に任せる
                _ => return Some(left),
            }
        }

        Some(left)
    }
}

// stmt
impl Parser<'_> {
    /// 現在のカーソル位置以降を代入文としてパースし,
    /// 代入文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let left = self.parse_ident()?;
        if !self.cur_token_is(Token::Assign) {
            return None;
        }
        self.bump();
        let right = self.parse_expr(Precedence::Lowest)?;
        Some(Stmt::Let(left, right))
    }

    /// 現在のカーソル位置以降をreturn文としてパースし,
    /// return文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let expr = self.parse_expr(Precedence::Lowest)?;
        Some(Stmt::Return(expr))
    }

    /// 現在のカーソル位置以降を式文としてパースし,
    /// 式文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        if self.next_token_is(Token::Semicolon) {
            self.bump();
        }
        Some(Stmt::Expr(expr))
    }

    /// 現在のカーソル位置以降を文としてパースし,
    /// 文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    /// 現在のカーソル位置以降をプログラムとしてパースし,
    /// プログラムの最後のトークンまでカーソルを進めます.
    pub fn parse(&mut self) -> Program {
        let mut program = vec![];

        while !self.cur_token_is(Token::Eof) {
            match self.parse_stmt() {
                Some(stmt) => program.push(stmt),
                None => {}
            }
            self.bump();
        }

        program
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_src(src: &str) -> Program {
        let mut parser = Parser::new(Lexer::new(src));
        parser.parse()
    }

    fn assert_expr(program: Program, expected: Vec<&str>) {
        assert_eq!(program.len(), expected.len());

        for (actual_stmt, formatted_expected_stmt) in program.iter().zip(expected) {
            assert_eq!(format!("{}", actual_stmt), formatted_expected_stmt);
        }
    }

    #[test]
    fn test_program() {
        let src = r#"
let num = 1;
return 2;
"#;

        let expected = vec![
            Stmt::Let(Ident("num".to_string()), Expr::Literal(Literal::Int(1))),
            Stmt::Return(Expr::Literal(Literal::Int(2))),
        ];

        let program = parse_src(src);

        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            println!("actual: {:?}, expected: {:?}", actual_stmt, expected_stmt);
            assert_eq!(actual_stmt, expected_stmt);
        }
    }

    #[test]
    fn test_numeric_operator() {
        let src = r#"
// basic
1 + 2 + 3;
1 + 2 - 3;
1 * 2 / 3 + 4 - 5;
1 - 2 + 3 / 4 * 5;

// paren
1 + (2 + 3) + 4;
1 + 2 * 3 + 4;

// unary
+++---1 + ---+++2;
-1 * 2;
-(1 * 2);
"#;

        let expected = vec![
            // basic
            "((1 + 2) + 3);",
            "((1 + 2) - 3);",
            "((((1 * 2) / 3) + 4) - 5);",
            "((1 - 2) + ((3 / 4) * 5));",
            // paren
            "((1 + (2 + 3)) + 4);",
            "((1 + (2 * 3)) + 4);",
            // unary
            "((+(+(+(-(-(-1)))))) + (-(-(-(+(+(+2)))))));",
            "((-1) * 2);",
            "(-(1 * 2));",
        ];

        let program = parse_src(src);

        assert_expr(program, expected);
    }
}
