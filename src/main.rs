#![deny(missing_debug_implementations)]
use std::io::{self, Read, BufReader, stdin};
use regex::Regex;
    // let input = if let Ok(ss) = String::from_utf8(input_) { ss } else { String::new() };

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Literal {
    Str(String),
    Int(u64),
    Bool(bool),
    None
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Token {
    Identifier(String),
    Literal(Literal),
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
    MulAssign,
    DivAssign,
    PowAssign,
    SingleQuote,
    DoubleQuote,
    Return,
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
            ">=" => Token::GtEq,
            "<=" => Token::LtEq,
            ">>" => Token::Shr,
            "<<" => Token::Shl,
            "^" => Token::Xor,
            "&" => Token::BinaryAnd,
            "|" => Token::BinaryOr,
            "&&" => Token::LogicalAnd,
            "||" => Token::LogicalOr,
            "true" => Token::Literal(Literal::Bool(true)),
            "false" => Token::Literal(Literal::Bool(false)),
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

    let mut literal = Literal::None;

    let mut integer_base: u32 = 10;
    let mut next_char_is_escaped = false;

    for c in input.chars() {
        match literal {
            Literal::Str(_) => {
                if c == '"' {
                    identifiers.push(Token::Literal(Literal::Str(current_identifier.to_owned())));
                    current_identifier = "";
                    literal = Literal::None;
                    continue;
                }

                ci = format!("{}{}", current_identifier, c);
                current_identifier = ci.as_ref();
                continue;
            },
            Literal::Int(_) => {
                match c {
                    '0'..='9' => {
                        ci = format!("{}{}", current_identifier, c);
                        current_identifier = ci.as_ref();
                        continue;
                    },
                    'a'..='f' | 'A'..='F' => {
                        if integer_base == 16 {
                            ci = format!("{}{}", current_identifier, c);
                            current_identifier = ci.as_ref();
                        } else {
                            unimplemented!()
                        }
                        continue;
                    }
                    'x' => {
                        if current_identifier.len() == 1 {
                            current_identifier = "";
                            integer_base = 16;
                        } else {
                            unimplemented!()
                        }
                        continue;
                    },
                    _ => {
                        identifiers.push(Token::Literal(Literal::Int(u64::from_str_radix(current_identifier, integer_base).unwrap())));
                        integer_base = 10;
                        current_identifier = "";
                        literal = Literal::None;
                    }
                } 
            },
            _ => {}
        }

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
            '"' => {
                literal = Literal::Str(String::new());
                continue;
            }
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

                if current_identifier == "*" {
                    identifiers.push(Token::MulAssign);
                    current_identifier = "";
                    continue;
                }

                if current_identifier == "/" {
                    identifiers.push(Token::DivAssign);
                    current_identifier = "";
                    continue;
                }

                if current_identifier == "<" {
                    identifiers.push(Token::LtEq);
                    current_identifier = "";
                    continue;
                }

                if current_identifier == ">" {
                    identifiers.push(Token::GtEq);
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
            '0'..='9' => {
                literal = Literal::Int(0);
                if current_identifier != "" {
                    identifiers.push(Token::new(current_identifier));
                    current_identifier = "";
                }
                ci = format!("{}{}", current_identifier, c);
                current_identifier = ci.as_ref();
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
