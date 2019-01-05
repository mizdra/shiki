use rustyline::error::ReadlineError;
use rustyline::Editor;
use shiki::{Evaluator, Lexer, Parser};

fn main() {
    let mut rl = Editor::<()>::new();
    let mut evaluator = Evaluator::new();

    loop {
        let readline = rl.readline("\x1b[34m>> \x1b[0m");
        match readline {
            Ok(line) => {
                let code = line.as_ref();
                rl.add_history_entry(code);

                let mut parser = Parser::new(Lexer::new(code));
                let program = parser.parse();

                match evaluator.eval(program) {
                    Ok(object) => println!("{}", object),
                    Err(error) => eprintln!("\x1b[31m{}\x1b[0m", error),
                };
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
