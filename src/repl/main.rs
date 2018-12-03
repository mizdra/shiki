use rustyline::error::ReadlineError;
use rustyline::Editor;
use shiki::lexer::token::Token;
use shiki::lexer::Lexer;

fn main() {
    let mut rl = Editor::<()>::new();

    loop {
        let readline = rl.readline("\x1b[34m>> \x1b[0m");
        match readline {
            Ok(line) => {
                let code = line.as_ref();
                rl.add_history_entry(code);
                let mut lexer = Lexer::new(code);

                let mut tokens = vec![];
                loop {
                    match lexer.next_token() {
                        Token::Eof => break,
                        token => tokens.push(token),
                    }
                }
                println!("{:?}", tokens);
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                println!("bye");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
