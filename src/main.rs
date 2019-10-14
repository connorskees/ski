#![deny(missing_debug_implementations)]
use std::io::{self, stdin};
use lexer::Lexer;

mod lexer;
    // let input = if let Ok(ss) = String::from_utf8(input_) { ss } else { String::new() };

fn main() -> io::Result<()> {
    let mut input = String::new();
    let mut lexer = Lexer::new();
    stdin().read_line(&mut input)?;

    let tokens = lexer.lex(&input);

    println!("{:?}", tokens);

    Ok(())
}
