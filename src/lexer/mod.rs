mod token;

pub use self::token::Token;

pub struct Lexer<'a> {
    input: &'a str, // 字句解析対象のソースコード
    pos: usize,     // 今読み込まれている文字の位置
    ch: u8,         // 今読み込まれている文字
}

impl Lexer<'_> {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input,
            pos: 0,
            ch: 0,
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

    /// カーソルを1文字分進めます.
    fn next_ch(&mut self) {
        if self.ch == 0 {
            return;
        }
        self.pos += 1;
        self.ch = self.get_ch(self.pos);
    }

    /// 次の文字が `ch` であるなら `true` を, そうでなければ `false` を返します.
    fn peek_ch_is(&self, ch: u8) -> bool {
        self.get_ch(self.pos + 1) == ch
    }

    /// 空白, タブ文字, 改行文字をスキップし, カーソルを進めます.
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

    /// 現在のカーソル位置以降を数値として字句解析し, カーソルを進めます.
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

    /// 現在のカーソル位置以降をキーワード (予約語/識別子) として字句解析し, カーソルを進めます.
    fn consume_keyword(&mut self) -> Token {
        let begin_pos = self.pos;
        self.next_ch(); // 先頭が数字であることは既に分かっているのでスキップ

        loop {
            match self.ch {
                b'a'...b'z' | b'A'...b'Z' | b'_' => self.next_ch(),
                _ => break,
            }
        }

        let end_pos = self.pos;

        let keyword_str = &self.input[begin_pos..end_pos];

        match keyword_str {
            "if" => Token::If,
            "else" => Token::Else,
            "let" => Token::Let,
            "return" => Token::Return,
            _ => Token::Ident(keyword_str.to_string()),
        }
    }

    /// 現在のカーソル位置以降を数値として字句解析し, カーソルを進めます.
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

    /// 現在のカーソル位置以降をコメントとしてスキップし, カーソルを進める
    fn skip_comment(&mut self) {
        // 先頭の `//` をスキップ
        self.next_ch();
        self.next_ch();
        loop {
            match self.ch {
                0 => break,
                b'\n' => {
                    self.next_ch(); // 改行文字をスキップ
                    break;
                }
                _ => self.next_ch(),
            }
        }
    }

    /// 現在のカーソル位置以降を任意のトークンとして字句解析し, カーソルを進めます.
    pub fn next_token(&mut self) -> Token {
        self.skip_space();

        let token = match self.ch {
            // Reseved keywords + Identifiers + Literals
            b'0'...b'9' => return self.consume_number(),
            b'a'...b'z' | b'A'...b'Z' | b'_' => return self.consume_keyword(),
            b'"' => return self.consume_string(),

            // Statements & Operators
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'*' => Token::Asterisk,
            b'/' => {
                if self.peek_ch_is(b'/') {
                    self.skip_comment();
                    return self.next_token();
                } else {
                    Token::Slash
                }
            }
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
            b'&' => {
                if self.peek_ch_is(b'&') {
                    self.next_ch();
                    Token::AndAnd
                } else {
                    Token::Invalid
                }
            }
            b'|' => {
                if self.peek_ch_is(b'|') {
                    self.next_ch();
                    Token::OrOr
                } else {
                    Token::Or
                }
            }

            // Delimiters
            b',' => Token::Comma,
            b';' => Token::Semicolon,
            b'(' => Token::Lparen,
            b')' => Token::Rparen,
            b'{' => Token::Lbrace,
            b'}' => Token::Rbrace,

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
<===!=>=&&||;
1++--1;

// Calculate `a ** b`
let pow = |a, b| {
  if b <= 1 {
    return a;
  } else {
    pow(a * b, b - 1)
  }
}
let result = pow(3, 3);

puts(result); // 27"#;

        let expected = vec![
            // 1 + 2+10;
            Token::Int(1),
            Token::Plus,
            Token::Int(2),
            Token::Plus,
            Token::Int(10),
            Token::Semicolon,
            // =+-*/%()<>!;
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
            // <===!=>=&&||;
            Token::LessThanEqual,
            Token::Equal,
            Token::NotEqual,
            Token::GreaterThanEqual,
            Token::AndAnd,
            Token::OrOr,
            Token::Semicolon,
            // 1++--1;
            Token::Int(1),
            Token::Plus,
            Token::Plus,
            Token::Minus,
            Token::Minus,
            Token::Int(1),
            Token::Semicolon,
            // let pow = ...
            Token::Let,
            Token::Ident("pow".to_string()),
            Token::Assign,
            Token::Or,
            Token::Ident("a".to_string()),
            Token::Comma,
            Token::Ident("b".to_string()),
            Token::Or,
            Token::Lbrace,
            Token::If,
            Token::Ident("b".to_string()),
            Token::LessThanEqual,
            Token::Int(1),
            Token::Lbrace,
            Token::Return,
            Token::Ident("a".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Ident("pow".to_string()),
            Token::Lparen,
            Token::Ident("a".to_string()),
            Token::Asterisk,
            Token::Ident("b".to_string()),
            Token::Comma,
            Token::Ident("b".to_string()),
            Token::Minus,
            Token::Int(1),
            Token::Rparen,
            Token::Rbrace,
            Token::Rbrace,
            // let result = ...
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("pow".to_string()),
            Token::Lparen,
            Token::Int(3),
            Token::Comma,
            Token::Int(3),
            Token::Rparen,
            Token::Semicolon,
            // puts(result);
            Token::Ident("puts".to_string()),
            Token::Lparen,
            Token::Ident("result".to_string()),
            Token::Rparen,
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
