#![deny(missing_debug_implementations)]
#![allow(dead_code, unused_imports)]
use std::io::{self, stdin};
use std::io::prelude::*;
use std::fs::File;
use lexer::Lexer;
use parser::Parser;

mod lexer;
mod errors;
mod ast;
mod parser;

fn main() -> io::Result<()> {
    let mut f = File::open("test.ski")?;
    let mut input = String::new();
    let mut lexer = Lexer::new();
    stdin().read_line(&mut input)?;
    // f.read_to_string(&mut input)?;

    let tokens = lexer.lex(&input).unwrap();
    
    println!("{:?}", &tokens);
    
    // let mut parser = Parser::new(tokens);
    // let x = parser.parse().unwrap();
    
    // println!("{:?}", x);

    Ok(())
}
