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
pub enum Keyword {
    Function,
    Let,
    Const,
    For,
    In,
    While,
    Loop,
    Return,
    If,
    Else
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Symbol {
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
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
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Token {
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol)
}

#[derive(Debug)]
pub struct Lexer {
    col: u16,
    row: u16
}

impl Token {
    pub fn new(token: &str) -> Token {
        match token {
            "fn" => Token::Keyword(Keyword::Function),
            "let" => Token::Keyword(Keyword::Let),
            "const" => Token::Keyword(Keyword::Const),
            "for" => Token::Keyword(Keyword::For),
            "while" => Token::Keyword(Keyword::While),
            "loop" => Token::Keyword(Keyword::Loop),
            "return" => Token::Keyword(Keyword::Return),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "in" => Token::Keyword(Keyword::In),
            "{" => Token::Symbol(Symbol::OpenBracket),
            "}" => Token::Symbol(Symbol::CloseBracket),
            "(" => Token::Symbol(Symbol::OpenParen),
            ")" => Token::Symbol(Symbol::CloseParen),
            "=" => Token::Symbol(Symbol::Equal),
            "==" => Token::Symbol(Symbol::DoubleEqual),
            ";" => Token::Symbol(Symbol::SemiColon),
            "'" => Token::Symbol(Symbol::SingleQuote),
            "\"" => Token::Symbol(Symbol::DoubleQuote),
            "+" => Token::Symbol(Symbol::Add),
            "-" => Token::Symbol(Symbol::Sub),
            "*" => Token::Symbol(Symbol::Mul),
            "/" => Token::Symbol(Symbol::Div),
            "**" => Token::Symbol(Symbol::Pow),
            "+=" => Token::Symbol(Symbol::AddAssign),
            "-=" => Token::Symbol(Symbol::SubAssign),
            ">" => Token::Symbol(Symbol::Gt),
            "<" => Token::Symbol(Symbol::Lt),
            ">=" => Token::Symbol(Symbol::GtEq),
            "<=" => Token::Symbol(Symbol::LtEq),
            ">>" => Token::Symbol(Symbol::Shr),
            "<<" => Token::Symbol(Symbol::Shl),
            "^" => Token::Symbol(Symbol::Xor),
            "&" => Token::Symbol(Symbol::BinaryAnd),
            "|" => Token::Symbol(Symbol::BinaryOr),
            "&&" => Token::Symbol(Symbol::LogicalAnd),
            "||" => Token::Symbol(Symbol::LogicalOr),
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
        // let mut next_char_is_escaped = false;

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
                        tokens.push(Token::Symbol(Symbol::DoubleEqual));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "+" {
                        tokens.push(Token::Symbol(Symbol::AddAssign));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "-" {
                        tokens.push(Token::Symbol(Symbol::SubAssign));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "*" {
                        tokens.push(Token::Symbol(Symbol::MulAssign));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "/" {
                        tokens.push(Token::Symbol(Symbol::DivAssign));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "<" {
                        tokens.push(Token::Symbol(Symbol::LtEq));
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == ">" {
                        tokens.push(Token::Symbol(Symbol::GtEq));
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
        match literal {
            LiteralType::Int => {
                tokens.push(Token::Literal(Literal::Int(u64::from_str_radix(current_identifier, integer_base).unwrap())));
            }
            LiteralType::Str => {
                tokens.push(Token::Literal(Literal::Str(current_identifier.to_owned())));
            }
            _ => {
                if current_identifier != "" {
                    tokens.push(Token::new(current_identifier));
                }
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;
    use super::Literal::*;
    use super::Symbol::*;
    #[test]
    fn test() {
        let mut lexer = Lexer::new();
        assert_eq!(
            lexer.lex("fn hi(){if 1==1{print(\"hi\");}}").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("hi")), Token::Symbol(OpenParen), Token::Symbol(CloseParen), Token::Symbol(OpenBracket), Token::Keyword(super::Keyword::If), Literal(Int(1)), Token::Symbol(DoubleEqual), Literal(Int(1)), Token::Symbol(OpenBracket), Identifier(String::from("print")), Token::Symbol(OpenParen), Literal(Str(String::from("hi"))), Token::Symbol(CloseParen), Token::Symbol(SemiColon), Token::Symbol(CloseBracket), Token::Symbol(CloseBracket))
        );
        assert_eq!(
            lexer.lex("fn hi() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("hi")), Token::Symbol(OpenParen), Token::Symbol(CloseParen), Token::Symbol(OpenBracket), Token::Keyword(super::Keyword::If), Literal(Int(1)), Token::Symbol(DoubleEqual), Literal(Int(1)), Token::Symbol(OpenBracket), Identifier(String::from("print")), Token::Symbol(OpenParen), Literal(Str(String::from("hi"))), Token::Symbol(CloseParen), Token::Symbol(SemiColon), Token::Symbol(CloseBracket), Token::Symbol(CloseBracket))
        );
        assert_eq!(
            lexer.lex("/**/fn hi/**//**/() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}// aje=  df d").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("hi")), Token::Symbol(OpenParen), Token::Symbol(CloseParen), Token::Symbol(OpenBracket), Token::Keyword(super::Keyword::If), Literal(Int(1)), Token::Symbol(DoubleEqual), Literal(Int(1)), Token::Symbol(OpenBracket), Identifier(String::from("print")), Token::Symbol(OpenParen), Literal(Str(String::from("hi"))), Token::Symbol(CloseParen), Token::Symbol(SemiColon), Token::Symbol(CloseBracket), Token::Symbol(CloseBracket))
        );
        assert_eq!(
            lexer.lex("1+1").unwrap(),
            vec!(Literal(Int(1)), Token::Symbol(Add), Literal(Int(1)))
        );
        assert_eq!(
            lexer.lex("1+1 ").unwrap(),
            vec!(Literal(Int(1)), Token::Symbol(Add), Literal(Int(1)))
        );
        assert_eq!(
            lexer.lex("1 + 1").unwrap(),
            vec!(Literal(Int(1)), Token::Symbol(Add), Literal(Int(1)))
        );
        assert_eq!(
            lexer.lex("let x = 1 + 1;").unwrap(),
            vec!(Token::Keyword(super::Keyword::Let), Identifier(String::from("x")), Token::Symbol(Equal), Literal(Int(1)), Token::Symbol(Add), Literal(Int(1)), Token::Symbol(SemiColon))
        );
        assert_eq!(
            lexer.lex("\"hi\"").unwrap(),
            vec!(Literal(Str(String::from("hi"))))
        );
        assert_eq!(
            lexer.lex("fn func()").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("func")), Token::Symbol(OpenParen), Token::Symbol(CloseParen))
        );
         assert_eq!(
            lexer.lex("fn func ()").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("func")), Token::Symbol(OpenParen), Token::Symbol(CloseParen))
        );
         assert_eq!(
            lexer.lex("fn func (  )").unwrap(),
            vec!(Token::Keyword(super::Keyword::Function), Identifier(String::from("func")), Token::Symbol(OpenParen), Token::Symbol(CloseParen))
        );
        assert_eq!(
            lexer.lex("| |").unwrap(),
            vec!(BinaryOr, BinaryOr)
        );
      
    }
}