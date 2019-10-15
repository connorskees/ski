use regex;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Literal {
    Str(String),
    Int(u64),
    Bool(bool),
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum LiteralType {
    Str,
    Int,
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
    In,
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

#[derive(Debug)]
pub struct Lexer {
    col: u16,
    row: u16
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
            "in" => Token::In,
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

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            row: 0,
            col: 0
        }
    }

    pub fn strip_comments(input: &str) -> Result<String, regex::Error> {
        let single_line = regex::Regex::new(r"//[^\n\r]*")?;
        let multi_line = regex::Regex::new(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/")?;

        Ok(
            single_line.replace_all(
                multi_line.replace_all(input, "").to_mut(), ""
            ).to_mut().to_string())
    }

    pub fn lex(&mut self, s: &str) -> Result<Vec<Token>, regex::Error> {
        let input = Lexer::strip_comments(s)?;
        let mut tokens: Vec<Token> = Vec::with_capacity(40);

        let mut ci: String;

        let mut current_identifier: &str = "";

        let mut literal = LiteralType::None;

        let mut integer_base: u32 = 10;
        let mut next_char_is_escaped = false;

        for c in input.chars() {
            self.col += 1;
            match literal {
                LiteralType::Str => {
                    if c == '"' {
                        tokens.push(Token::Literal(Literal::Str(current_identifier.to_owned())));
                        current_identifier = "";
                        literal = LiteralType::None;
                        continue;
                    }

                    ci = format!("{}{}", current_identifier, c);
                    current_identifier = ci.as_ref();
                    continue;
                },
                LiteralType::Int => {
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
                            tokens.push(Token::Literal(Literal::Int(u64::from_str_radix(current_identifier, integer_base).unwrap())));
                            integer_base = 10;
                            current_identifier = "";
                            literal = LiteralType::None;
                        }
                    } 
                },
                _ => {}
            }

            match c {
                ' ' | '\r' | '\t' => {
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                        current_identifier = "";
                    }
                },
                '\n' => {
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                        current_identifier = "";
                    }
                    self.row += 1;
                    self.col = 0;
                }
                '{' | '}' | '(' | ')' | ';' | '^' => {
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                        current_identifier = "";
                    }
                    tokens.push(Token::new(&c.to_string()));
                },
                '+' => {
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                    }
                    current_identifier = "+";
                },
                '-' => {
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                    }
                    current_identifier = "-";
                },
                '"' => {
                    literal = LiteralType::Str;
                    continue;
                }
                '=' => {
                    if current_identifier == "=" {
                        tokens.push(Token::DoubleEqual);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "+" {
                        tokens.push(Token::AddAssign);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "-" {
                        tokens.push(Token::SubAssign);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "*" {
                        tokens.push(Token::MulAssign);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "/" {
                        tokens.push(Token::DivAssign);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "<" {
                        tokens.push(Token::LtEq);
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == ">" {
                        tokens.push(Token::GtEq);
                        current_identifier = "";
                        continue;
                    }


                    
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                    }

                    current_identifier = "=";
                }
                '&' => {
                    double_identifier!("&", current_identifier, tokens);
                }
                '|' => {
                    double_identifier!("|", current_identifier, tokens);
                }
                '*' => {
                    double_identifier!("*", current_identifier, tokens);
                }
                '/' => {
                    double_identifier!("/", current_identifier, tokens);
                }
                '>' => {
                    double_identifier!(">", current_identifier, tokens);
                }
                '<' => {
                    double_identifier!("<", current_identifier, tokens);
                }
                '0'..='9' => {
                    literal = LiteralType::Int;
                    if current_identifier != "" {
                        tokens.push(Token::new(current_identifier));
                        current_identifier = "";
                    }
                    ci = format!("{}{}", current_identifier, c);
                    current_identifier = ci.as_ref();
            }
                _ => {
                    match current_identifier {
                        "=" | "&" | "|" | "*" | "/" | "<" | ">" | "+" | "-" => {
                            tokens.push(Token::new(current_identifier));
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
            tokens.push(Token::new(current_identifier));
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;
    use super::Literal::*;
    #[test]
    fn test() {
        let mut lexer = Lexer::new();
        assert_eq!(
            lexer.lex("fn hi(){if 1==1{print(\"hi\");}}").unwrap(),
            vec!(Function, Identifier(String::from("hi")), OpenParen, CloseParen, OpenBracket, If, Literal(Int(1)), DoubleEqual, Literal(Int(1)), OpenBracket, Identifier(String::from("print")), OpenParen, Literal(Str(String::from("hi"))), CloseParen, SemiColon, CloseBracket, CloseBracket)
        );
        assert_eq!(
            lexer.lex("fn hi() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}").unwrap(),
            vec!(Function, Identifier(String::from("hi")), OpenParen, CloseParen, OpenBracket, If, Literal(Int(1)), DoubleEqual, Literal(Int(1)), OpenBracket, Identifier(String::from("print")), OpenParen, Literal(Str(String::from("hi"))), CloseParen, SemiColon, CloseBracket, CloseBracket)
        );
        assert_eq!(
            lexer.lex("/**/fn hi/**//**/() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}// aje=  df d").unwrap(),
            vec!(Function, Identifier(String::from("hi")), OpenParen, CloseParen, OpenBracket, If, Literal(Int(1)), DoubleEqual, Literal(Int(1)), OpenBracket, Identifier(String::from("print")), OpenParen, Literal(Str(String::from("hi"))), CloseParen, SemiColon, CloseBracket, CloseBracket)
        );
    }
}