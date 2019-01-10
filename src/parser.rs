use crate::{ast::*, Lexer, Token};

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

    fn expect(&mut self, token: Token) -> Option<()> {
        if self.cur_token_is(token) {
            self.bump();
            Some(())
        } else {
            None // TODO: エラー報告
        }
    }

    fn expect_next(&mut self, token: Token) -> Option<()> {
        if self.next_token_is(token) {
            self.bump();
            Some(())
        } else {
            None // TODO: エラー報告
        }
    }

    fn eat(&mut self, token: Token) -> bool {
        if self.cur_token_is(token) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_next(&mut self, token: Token) -> bool {
        if self.next_token_is(token) {
            self.bump();
            true
        } else {
            false
        }
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
            Token::AndAnd | Token::OrOr => Precedence::AndAnd,
            _ => Precedence::Lowest,
        }
    }

    fn cur_token_precedence(&self) -> Precedence {
        Self::token_precedence_is(&self.cur_token)
    }

    fn next_token_precedence(&self) -> Precedence {
        Self::token_precedence_is(&self.next_token)
    }
}

// ident
impl Parser<'_> {
    /// 現在のカーソル位置以降を識別子としてパースし,
    /// 識別子の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Ident` であるとする)
    fn parse_ident(&mut self) -> Option<Ident> {
        if let Token::Ident(ref ident) = self.cur_token {
            Some(Ident(ident.clone()))
        } else {
            panic!();
        }
    }

    /// 現在のカーソル位置以降をパラメータリストとしてパースし,
    /// 識別子の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `|` または `||` であるとする)
    fn parse_params(&mut self) -> Option<Vec<Ident>> {
        if self.cur_token_is(Token::OrOr) {
            return Some(vec![]);
        }
        self.bump(); // skip `|`
        let mut params = vec![];
        while !self.cur_token_is(Token::Or) {
            params.push(self.parse_ident()?);
            self.bump();
            self.eat(Token::Comma);
        }
        Some(params)
    }

    /// 現在のカーソル位置以降を式のリストとしてパースし,
    /// 識別子の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `(` であるとする)
    fn parse_expr_list(&mut self) -> Option<Vec<Expr>> {
        self.bump();
        let mut list = vec![];
        while !self.cur_token_is(Token::Rparen) {
            list.push(self.parse_expr(Precedence::Lowest)?);
            self.bump();
            self.eat(Token::Comma);
        }
        Some(list)
    }
}

// expr
impl Parser<'_> {
    /// 現在のカーソル位置以降を識別子式としてパースし,
    /// 識別子式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Ident` であるとする)
    fn parse_ident_expr(&mut self) -> Option<Expr> {
        let ident = self.parse_ident()?;
        Some(Expr::Ident(ident))
    }

    /// 現在のカーソル位置以降を整数リテラル式としてパースし,
    /// 整数リテラル式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Int` であるとする)
    fn parse_int_expr(&mut self) -> Option<Expr> {
        if let Token::Int(int) = self.cur_token {
            Some(Expr::Literal(Literal::Int(int)))
        } else {
            panic!();
        }
    }

    /// 現在のカーソル位置以降を文字列式としてパースし,
    /// 文字列式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `String` であるとする)
    fn parse_string_expr(&mut self) -> Option<Expr> {
        if let Token::String(ref string) = self.cur_token {
            Some(Expr::Literal(Literal::String(string.clone())))
        } else {
            panic!();
        }
    }

    /// 現在のカーソル位置以降を真偽値式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Bool` であるとする)
    fn parse_bool_expr(&mut self) -> Option<Expr> {
        if let Token::Bool(value) = self.cur_token {
            Some(Expr::Literal(Literal::Bool(value)))
        } else {
            panic!();
        }
    }

    /// 現在のカーソル位置以降を単項演算子の式としてパースし,
    /// 単項演算子の式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Prefix::*` であるとする)
    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.cur_token {
            Token::Bang => Prefix::Not,
            Token::Plus => Prefix::Plus,
            Token::Minus => Prefix::Minus,
            _ => panic!(),
        };
        self.bump();
        let expr = self.parse_expr(Precedence::Prefix)?;
        Some(Expr::Prefix(prefix, Box::new(expr)))
    }

    /// 現在のカーソル位置以降を括弧による式としてパースし,
    /// 括弧による式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `(` であるとする)
    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.bump();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Rparen)?;
        Some(expr)
    }

    /// 現在のカーソル位置が中置演算子の式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// 現在のカーソル位置が中置演算子でない場合など,
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `Infix::*` であるとする)
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
            Token::AndAnd => Infix::AndAnd,
            Token::OrOr => Infix::OrOr,
            _ => panic!(),
        };
        let precedence = self.cur_token_precedence();
        self.bump();

        let expr = self.parse_expr(precedence)?;
        Some(Expr::Infix(infix, Box::new(left), Box::new(expr)))
    }

    /// 現在のカーソル位置以降をラムダ式呼び出しとしてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `(` であるとする)
    fn parse_call_expr(&mut self, left: Expr) -> Option<Expr> {
        let args = self.parse_expr_list()?;

        Some(Expr::Call(Box::new(left), args))
    }

    /// 現在のカーソル位置以降をブロック式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `{` であるとする)
    fn parse_block_expr(&mut self) -> Option<Expr> {
        let block_stmt = self.parse_block_stmt()?;
        Some(Expr::Block(block_stmt))
    }

    /// 現在のカーソル位置以降をラムダ式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `|` または `||` であるとする)
    fn parse_lambda_expr(&mut self) -> Option<Expr> {
        let params = self.parse_params()?;
        self.bump();
        let body = self.parse_expr(Precedence::Lowest)?;
        Some(Expr::Lambda(params, Box::new(body)))
    }

    /// 現在のカーソル位置以降をif式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `if` であるとする)
    fn parse_if_expr(&mut self) -> Option<Expr> {
        self.bump(); // skip `if`

        let cond = Box::new(self.parse_expr(Precedence::Lowest)?);
        self.bump();
        let consequence = self.parse_block_stmt()?;

        let alternative = if self.next_token_is(Token::Else) {
            // } else if <expr> <block_stmt>
            // ^ cur_token

            self.bump(); // skip `}`
            self.bump(); // skip `else`
            if self.cur_token_is(Token::If) {
                Some(vec![Stmt::Expr(self.parse_if_expr()?)])
            } else {
                Some(self.parse_block_stmt()?)
            }
        } else {
            None
        };
        Some(Expr::If(cond, consequence, alternative))
    }

    /// 現在のカーソル位置以降をwhile式としてパースし,
    /// 式の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `while` であるとする)
    fn parse_while_expr(&mut self) -> Option<Expr> {
        self.bump(); // skip `while`
        let cond = Box::new(self.parse_expr(Precedence::Lowest)?);
        self.bump();
        let body = self.parse_block_stmt()?;
        Some(Expr::While(cond, body))
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
            Token::Bool(_) => self.parse_bool_expr()?,
            Token::Bang | Token::Plus | Token::Minus => self.parse_prefix_expr()?,
            Token::Lparen => match self.next_token {
                Token::Rparen => {
                    self.bump();
                    Expr::Literal(Literal::Unit)
                }
                _ => self.parse_grouped_expr()?,
            },
            Token::Lbrace => self.parse_block_expr()?,
            Token::Or | Token::OrOr => self.parse_lambda_expr()?,
            Token::If => self.parse_if_expr()?,
            Token::While => self.parse_while_expr()?,
            _ => return None, // TODO: エラー報告
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
                | Token::GreaterThanEqual
                | Token::AndAnd
                | Token::OrOr => {
                    self.bump();
                    left = self.parse_infix_expr(left)?;
                }
                Token::Lparen => {
                    self.bump();
                    left = self.parse_call_expr(left)?;
                }
                // return して呼び出し元に残りのトークンのパースを任せる
                _ => return Some(left),
            }
        }

        Some(left)
    }
}

// stmt
impl Parser<'_> {
    /// 現在のカーソル位置以降を宣言文としてパースし,
    /// 宣言文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `let` であるとする)
    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.bump(); // skip `let`
        let left = self.parse_ident()?;
        self.bump();
        self.expect(Token::Assign)?; // skip `=`
        let right = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Semicolon)?;
        Some(Stmt::Let(left, right))
    }

    /// 現在のカーソル位置以降を代入文としてパースし,
    /// 代入文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `ident`, `next_token` が `=` であるとする)
    fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        let left = self.parse_ident()?;
        self.bump();
        self.bump(); // skip `=`
        let right = self.parse_expr(Precedence::Lowest)?;
        self.expect_next(Token::Semicolon)?;
        Some(Stmt::Assign(left, right))
    }

    /// 現在のカーソル位置以降をreturn文としてパースし,
    /// return文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `return` であるとする)
    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.bump();
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.eat_next(Token::Semicolon);
        Some(Stmt::Return(expr))
    }

    /// 現在のカーソル位置以降を式文としてパースし,
    /// 式文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.eat_next(Token::Semicolon);
        Some(Stmt::Expr(expr))
    }

    /// 現在のカーソル位置以降を文としてパースし,
    /// 文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_stmt(),
            Token::Ident(_) if self.next_token_is(Token::Assign) => self.parse_assign_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    /// 現在のカーソル位置以降を複文としてパースし,
    /// 文の最後のトークンまでカーソルを進めます.
    /// パースに失敗した場合は None を返します.
    /// (ただし `cur_token` が `{` であるとする)
    fn parse_block_stmt(&mut self) -> Option<Vec<Stmt>> {
        self.bump();
        let mut result = vec![];
        while !self.cur_token_is(Token::Rbrace) {
            result.push(self.parse_stmt()?);
            self.bump();
        }
        Some(result)
    }

    /// 現在のカーソル位置以降をプログラムとしてパースし,
    /// プログラムの最後のトークンまでカーソルを進めます.
    pub fn parse(&mut self) -> Program {
        let mut program = vec![];

        while !self.cur_token_is(Token::Eof) {
            match self.parse_stmt() {
                Some(stmt) => program.push(stmt),
                None => {} // TODO: エラー復帰
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
    fn test_stmt() {
        let src = r#"
let num = 1;
val = 2;
3;
return 4;
"#;

        let expected = vec![
            Stmt::Let(Ident("num".to_string()), Expr::Literal(Literal::Int(1))),
            Stmt::Assign(Ident("val".to_string()), Expr::Literal(Literal::Int(2))),
            Stmt::Expr(Expr::Literal(Literal::Int(3))),
            Stmt::Return(Expr::Literal(Literal::Int(4))),
        ];

        let program = parse_src(src);

        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            assert_eq!(actual_stmt, expected_stmt);
        }
    }

    #[test]
    fn test_literal() {
        let src = r#"
0;
"str";
true;
false;
();
"#;

        let expected = vec![
            Stmt::Expr(Expr::Literal(Literal::Int(0))),
            Stmt::Expr(Expr::Literal(Literal::String("str".to_string()))),
            Stmt::Expr(Expr::Literal(Literal::Bool(true))),
            Stmt::Expr(Expr::Literal(Literal::Bool(false))),
            Stmt::Expr(Expr::Literal(Literal::Unit)),
        ];

        let program = parse_src(src);

        assert_eq!(program.len(), expected.len());
        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            assert_eq!(actual_stmt, expected_stmt);
        }
    }

    #[test]
    fn test_operator() {
        let src = r#"
// prefix
-1;
!true;

// infix
1 + 2;
1 - 2;
1 * 2;
1 / 2;
1 == 2;
1 != 2;
1 >= 2;
1 > 2;
1 <= 2;
1 < 2;
true && false;
true || false;
"#;

        let expected = vec![
            // prefix
            "(-1);",
            "(!true);",
            // infix
            "(1 + 2);",
            "(1 - 2);",
            "(1 * 2);",
            "(1 / 2);",
            "(1 == 2);",
            "(1 != 2);",
            "(1 >= 2);",
            "(1 > 2);",
            "(1 <= 2);",
            "(1 < 2);",
            "(true && false);",
            "(true || false);",
        ];

        let program = parse_src(src);

        assert_eq!(program.len(), expected.len());
        assert_expr(program, expected);
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

        assert_eq!(program.len(), expected.len());
        assert_expr(program, expected);
    }

    #[test]
    fn test_lambda_expr() {
        let src = r#"
// lambda expression
|| {
    return 0;
};
|| {
    0
};
|| 0;
|x| 0;
|x,| 0;
|a, b| 0;
|a, b,| 0;

// first-class object
let val = || 0;

// call expression
(|| 0)(1);
func(1)(2);
add(1, 2);
add(1, 2, );
"#;

        let expected = vec![
            // implementation expression
            Stmt::Expr(Expr::Lambda(
                vec![],
                Box::new(Expr::Block(vec![Stmt::Return(Expr::Literal(
                    Literal::Int(0),
                ))])),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![],
                Box::new(Expr::Block(vec![Stmt::Expr(Expr::Literal(Literal::Int(
                    0,
                )))])),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![],
                Box::new(Expr::Literal(Literal::Int(0))),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![Ident("x".to_string())],
                Box::new(Expr::Literal(Literal::Int(0))),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![Ident("x".to_string())],
                Box::new(Expr::Literal(Literal::Int(0))),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![Ident("a".to_string()), Ident("b".to_string())],
                Box::new(Expr::Literal(Literal::Int(0))),
            )),
            Stmt::Expr(Expr::Lambda(
                vec![Ident("a".to_string()), Ident("b".to_string())],
                Box::new(Expr::Literal(Literal::Int(0))),
            )),
            // first-class object
            Stmt::Let(
                Ident("val".to_string()),
                Expr::Lambda(vec![], Box::new(Expr::Literal(Literal::Int(0)))),
            ),
            // call expression
            Stmt::Expr(Expr::Call(
                Box::new(Expr::Lambda(
                    vec![],
                    Box::new(Expr::Literal(Literal::Int(0))),
                )),
                vec![Expr::Literal(Literal::Int(1))],
            )),
            Stmt::Expr(Expr::Call(
                Box::new(Expr::Call(
                    Box::new(Expr::Ident(Ident("func".to_string()))),
                    vec![Expr::Literal(Literal::Int(1))],
                )),
                vec![Expr::Literal(Literal::Int(2))],
            )),
            Stmt::Expr(Expr::Call(
                Box::new(Expr::Ident(Ident("add".to_string()))),
                vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                ],
            )),
            Stmt::Expr(Expr::Call(
                Box::new(Expr::Ident(Ident("add".to_string()))),
                vec![
                    Expr::Literal(Literal::Int(1)),
                    Expr::Literal(Literal::Int(2)),
                ],
            )),
        ];

        let program = parse_src(src);

        assert_eq!(program.len(), expected.len());
        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            assert_eq!(actual_stmt, expected_stmt);
        }
    }

    #[test]
    fn test_if_expr() {
        let src = r#"
if 1 { 10; };
if 1 { 10; } else      { 20; };
if 1 { 10; } else if 2 { 20; };
if 1 { 10; } else if 2 { 20; } else { 30; };
"#;

        let expected = vec![
            // implementation expression
            Stmt::Expr(Expr::If(
                Box::new(Expr::Literal(Literal::Int(1))),
                vec![Stmt::Expr(Expr::Literal(Literal::Int(10)))],
                None,
            )),
            Stmt::Expr(Expr::If(
                Box::new(Expr::Literal(Literal::Int(1))),
                vec![Stmt::Expr(Expr::Literal(Literal::Int(10)))],
                Some(vec![Stmt::Expr(Expr::Literal(Literal::Int(20)))]),
            )),
            Stmt::Expr(Expr::If(
                Box::new(Expr::Literal(Literal::Int(1))),
                vec![Stmt::Expr(Expr::Literal(Literal::Int(10)))],
                Some(vec![Stmt::Expr(Expr::If(
                    Box::new(Expr::Literal(Literal::Int(2))),
                    vec![Stmt::Expr(Expr::Literal(Literal::Int(20)))],
                    None,
                ))]),
            )),
            Stmt::Expr(Expr::If(
                Box::new(Expr::Literal(Literal::Int(1))),
                vec![Stmt::Expr(Expr::Literal(Literal::Int(10)))],
                Some(vec![Stmt::Expr(Expr::If(
                    Box::new(Expr::Literal(Literal::Int(2))),
                    vec![Stmt::Expr(Expr::Literal(Literal::Int(20)))],
                    Some(vec![Stmt::Expr(Expr::Literal(Literal::Int(30)))]),
                ))]),
            )),
        ];

        let program = parse_src(src);

        assert_eq!(program.len(), expected.len());
        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            assert_eq!(actual_stmt, expected_stmt);
        }
    }

    #[test]
    fn test_while_expr() {
        let src = r#"
while 1 {
    10;
};
"#;

        let expected = vec![
            // implementation expression
            Stmt::Expr(Expr::While(
                Box::new(Expr::Literal(Literal::Int(1))),
                vec![Stmt::Expr(Expr::Literal(Literal::Int(10)))],
            )),
        ];

        let program = parse_src(src);

        assert_eq!(program.len(), expected.len());
        for (actual_stmt, expected_stmt) in program.iter().zip(&expected) {
            assert_eq!(actual_stmt, expected_stmt);
        }
    }
}
