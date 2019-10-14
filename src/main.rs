#![deny(missing_debug_implementations)]
use std::io::{self, stdin};
use std::io::prelude::*;
use std::fs::File;
use lexer::Lexer;

mod lexer;

fn main() -> io::Result<()> {
    let mut f = File::open("test.ski")?;
    let mut input = String::new();
    let mut lexer = Lexer::new();
    // stdin().read_line(&mut input)?;
    f.read_to_string(&mut input)?;

    let tokens = lexer.lex(&input);

    println!("{:?}", tokens);

    Ok(())
}
