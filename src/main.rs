#![deny(missing_debug_implementations)]
use std::io::{self, Read, BufReader, stdin};
use regex::Regex;
    // let input = if let Ok(ss) = String::from_utf8(input_) { ss } else { String::new() };

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Token {
    Identifier(String),
    OpenBracket,
    CloseBracket,
    Function,
    OpenParen,
    CloseParen,
    Let,
    Const,
    For,
    While,
    Loop,
    SemiColon,
    DoubleEqual,
    Equal,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    AddAssign,
    SubAssign,
    SingleQuote,
    DoubleQuote,
    Return,
    Integer(u32),
    Gt,
    Lt,
    GtEq,
    LtEq,
    Shr,
    Shl,
    Xor,
    LogicalAnd,
    LogicalOr,
    BinaryAnd,
    BinaryOr,
    If,
    Else
}

impl Token {
    pub fn new(token: &str) -> Token {
        match token {
            "{" => Token::OpenBracket,
            "}" => Token::CloseBracket,
            "fn" => Token::Function,
            "(" => Token::OpenParen,
            ")" => Token::CloseParen,
            "let" => Token::Let,
            "const" => Token::Const,
            "for" => Token::For,
            "while" => Token::While,
            "loop" => Token::Loop,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "=" => Token::Equal,
            "==" => Token::DoubleEqual,
            ";" => Token::SemiColon,
            "'" => Token::SingleQuote,
            "\"" => Token::DoubleQuote,
            "+" => Token::Add,
            "-" => Token::Sub,
            "*" => Token::Mul,
            "/" => Token::Div,
            "**" => Token::Pow,
            "+=" => Token::AddAssign,
            "-=" => Token::SubAssign,
            ">" => Token::Gt,
            "<" => Token::Lt,
            ">>" => Token::Shr,
            "<<" => Token::Shl,
            "^" => Token::Xor,
            "&" => Token::BinaryAnd,
            "|" => Token::BinaryOr,
            "&&" => Token::LogicalAnd,
            "||" => Token::LogicalOr,
            _ => {
                Token::Identifier(token.to_owned())
            }
        }
    }
}

macro_rules! double_identifier {
    ( $i:literal, $cur:ident, $idents:ident ) => {
        if $cur == $i {
            $idents.push(Token::new(concat!($i, $i)));
            $cur = "";
            continue;
        }
        
        if $cur != "" {
            $idents.push(Token::new($cur));
        }

        $cur = $i;
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    stdin().read_line(&mut input)?;

    let mut identifiers: Vec<Token> = Vec::with_capacity(40);

    let mut ci: String;

    let mut current_identifier: &str = "";

    for c in input.chars() {
        match c {
            ' ' | '\r' | '\n' | '\t' => {
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                    current_identifier = "";
                }
            },
            '{' | '}' | '(' | ')' | ';' | '^' => {
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                    current_identifier = "";
                }
                identifiers.push(Token::new(&c.to_string()));
            },
            '+' => {
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                }
                current_identifier = "+";
            },
            '-' => {
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                }
                current_identifier = "-";
            },
            '=' => {
                if current_identifier == "=" {
                    identifiers.push(Token::DoubleEqual);
                    current_identifier = "";
                    continue;
                }

                if current_identifier == "+" {
                    identifiers.push(Token::AddAssign);
                    current_identifier = "";
                    continue;
                }

                if current_identifier == "-" {
                    identifiers.push(Token::SubAssign);
                    current_identifier = "";
                    continue;
                }
                
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                }

                current_identifier = "=";
            }
            '&' => {
                double_identifier!("&", current_identifier, identifiers);
            }
            '|' => {
                double_identifier!("|", current_identifier, identifiers);
            }
            '*' => {
                double_identifier!("*", current_identifier, identifiers);
            }
            '/' => {
                double_identifier!("/", current_identifier, identifiers);
            }
            '>' => {
                double_identifier!(">", current_identifier, identifiers);
            }
            '<' => {
                double_identifier!("<", current_identifier, identifiers);
            }
            _ => {
                match current_identifier {
                    "=" | "&" | "|" | "*" | "/" | "<" | ">" | "+" | "-" => {
                        identifiers.push(Token::new(current_identifier));
                        current_identifier = "";
                    }
                    _ => {}
                }
                ci = format!("{}{}", current_identifier, c);
                current_identifier = ci.as_ref();
            }
        }
    }

    if current_identifier != "" {
        identifiers.push(Token::new(current_identifier));
    }

    println!("{:?}", identifiers);

    Ok(())
}
