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

    fn skip_semicolons(&mut self) {
        while self.cur_token_is(Token::Semicolon) {
            self.bump();
        }
    }
}

// ident
impl Parser<'_> {
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
    fn parse_expr(&mut self) -> Option<Expr> {
        self.bump();
        Some(Expr::Literal(Literal::Int(1)))
    }
}

// stmt
impl Parser<'_> {
    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let left = self.parse_ident()?;
        if !self.cur_token_is(Token::Assign) {
            return None;
        }
        self.bump();
        let right = self.parse_expr()?;
        Some(Stmt::Let(left, right))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let expr = self.parse_expr()?;
        Some(Stmt::Return(expr))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        println!("Found unsupported token: {:?}", self.cur_token);
        unimplemented!();
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = vec![];

        while !self.cur_token_is(Token::Eof) {
            match self.parse_stmt() {
                Some(stmt) => program.push(stmt),
                None => {}
            }
            self.skip_semicolons();
        }

        program
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program() {
        let program = r#"
let num = 1;
return 2;
"#;

        let expected = vec![
            Stmt::Let(Ident("num".to_string()), Expr::Literal(Literal::Int(1))),
            Stmt::Return(Expr::Literal(Literal::Int(1))),
        ];

        let lexer = Lexer::new(program);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            println!("actual: {:?}, expected: {:?}", actual_stmt, expected_stmt);
            assert_eq!(actual_stmt, expected_stmt);
        }
    }
}
