pub mod token;

use crate::lexer::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            pos: 0, // 今読み込まれている文字の位置
            ch: 0,  // 今読み込まれている文字
        };

        lexer.ch = lexer.get_ch(0);
        lexer
    }

    /// ソースコードの `pos` の位置にある文字を返します.
    fn get_ch(&self, pos: usize) -> u8 {
        if pos < self.input.len() {
            self.input.as_bytes()[pos]
        } else {
            0
        }
    }

    fn next_ch(&mut self) {
        if self.ch == 0 {
            return;
        }
        self.pos += 1;
        self.ch = self.get_ch(self.pos);
    }

    fn peek_ch_is(&self, ch: u8) -> bool {
        self.get_ch(self.pos + 1) == ch
    }

    fn skip_space(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' => {
                    self.next_ch(); // 空白をスキップし, pos を次の文字に合わせる
                }
                _ => {
                    break;
                }
            }
        }
    }

    /// 現在のカーソル位置以降を数値として字句解析し, カーソルを進める
    fn consume_number(&mut self) -> Token {
        let begin_pos = self.pos;
        self.next_ch(); // 先頭が数字であることは既に分かっているのでスキップ

        while let b'0'...b'9' = self.ch {
            self.next_ch()
        }
        let end_pos = self.pos;

        let number_str = &self.input[begin_pos..end_pos];
        Token::Int(number_str.parse().unwrap())
    }

    /// 現在のカーソル位置以降を識別子として字句解析し, カーソルを進める
    fn consume_ident(&mut self) -> Token {
        let begin_pos = self.pos;
        self.next_ch(); // 先頭が数字であることは既に分かっているのでスキップ

        loop {
            match self.ch {
                b'a'...b'z' | b'A'...b'Z' | b'_' => self.next_ch(),
                _ => break,
            }
        }

        let end_pos = self.pos;

        let ident_str = &self.input[begin_pos..end_pos];

        match ident_str {
            "let" => Token::Let,
            _ => Token::Ident(ident_str.to_string()),
        }
    }

    /// 現在のカーソル位置以降を数値として字句解析し, カーソルを進める
    fn consume_string(&mut self) -> Token {
        self.next_ch(); // 先頭の `"` をスキップ
        let begin_pos = self.pos;

        while b'"' != self.ch {
            self.next_ch()
        }
        let end_pos = self.pos;
        self.next_ch(); // 末尾の `"` をスキップ

        let string_str = &self.input[begin_pos..end_pos];
        Token::String(string_str.to_string())
    }

    /// 現在のカーソル位置以降を任意のトークンとして字句解析し, カーソルを進める
    pub fn next_token(&mut self) -> Token {
        self.skip_space();

        let token = match self.ch {
            // Identifiers + literals
            b'0'...b'9' => return self.consume_number(),
            b'a'...b'z' | b'A'...b'Z' | b'_' => return self.consume_ident(),
            b'"' => return self.consume_string(),

            // Statements & Operators
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'!' => {
                if self.peek_ch_is(b'=') {
                    self.next_ch();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            b'%' => Token::Percent,
            b'=' => {
                if self.peek_ch_is(b'=') {
                    self.next_ch();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            b'<' => {
                if self.peek_ch_is(b'=') {
                    self.next_ch();
                    Token::LessThanEqual
                } else {
                    Token::LessThan
                }
            }
            b'>' => {
                if self.peek_ch_is(b'=') {
                    self.next_ch();
                    Token::GreaterThanEqual
                } else {
                    Token::GreaterThan
                }
            }

            // Delimiters
            b';' => Token::Semicolon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,

            // Control tokens
            0 => return Token::Eof,
            _ => Token::Invalid,
        };

        self.next_ch(); // 次の文字へとカーソルを進める

        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_program() {
        let program = r#"
1 + 2+10;
=+-*/%()<>!;
<===!=>=;
1++--1;

let num = 1 + 2;
let str = "Hello " + "World!";
"#;

        let expected = vec![
            Token::Int(1),
            Token::Plus,
            Token::Int(2),
            Token::Plus,
            Token::Int(10),
            Token::Semicolon,
            Token::Assign,
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Percent,
            Token::Lparen,
            Token::Rparen,
            Token::LessThan,
            Token::GreaterThan,
            Token::Bang,
            Token::Semicolon,
            Token::LessThanEqual,
            Token::Equal,
            Token::NotEqual,
            Token::GreaterThanEqual,
            Token::Semicolon,
            Token::Int(1),
            Token::Plus,
            Token::Plus,
            Token::Minus,
            Token::Minus,
            Token::Int(1),
            Token::Semicolon,
            Token::Let,
            Token::Ident("num".to_string()),
            Token::Assign,
            Token::Int(1),
            Token::Plus,
            Token::Int(2),
            Token::Semicolon,
            Token::Let,
            Token::Ident("str".to_string()),
            Token::Assign,
            Token::String("Hello ".to_string()),
            Token::Plus,
            Token::String("World!".to_string()),
            Token::Semicolon,
            Token::Eof,
            Token::Eof,
        ];

        let mut lexer = Lexer::new(program);

        for expected_token in expected {
            let actual_token = lexer.next_token();
            println!("left: {:?}, right: {:?}", actual_token, expected_token);
            assert_eq!(actual_token, expected_token);
        }
    }
}
