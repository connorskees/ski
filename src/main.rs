#![deny(missing_debug_implementations)]
use std::io::{self, Read, BufReader, stdin};
    // let input = if let Ok(ss) = String::from_utf8(input_) { ss } else { String::new() };

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Token {
    Identifier(String),
    OpenBracket,
    CloseBracket
}

impl Token {
    pub fn new(token: String) -> Token {
        match token.as_ref() {
            "{" => Token::OpenBracket,
            "}" => Token::CloseBracket,
            _ => Token::Identifier(token.to_owned()),
        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    stdin().read_line(&mut input)?;

    let mut indentifiers: Vec<Token> = Vec::with_capacity(40);

    let mut current_indentifier: String = String::new();

    for c in input.chars() {
        match c {
            ' ' => {
                indentifiers.push(Token::new(current_indentifier));
                current_indentifier = String::new();
            }
            '{' | '}' => {
                indentifiers.push(Token::new(c.to_string()));
                current_indentifier = String::new();
            },
            _ => {
                current_indentifier += &c.to_string();
            }
        }
        // println!("{:?}", c, indentifiers);
    }
    println!("{:?}", indentifiers);

    Ok(())
}
