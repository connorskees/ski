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
pub enum TokenKind {
    Identifier(String),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol)
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub struct Pos {
    row: u16,
    col: u16
}

impl Pos {
    pub fn new() -> Pos {
        Pos {
            row: 0,
            col: 0
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    pos: Pos,
}

impl TokenKind {
    pub fn new(token: &str) -> TokenKind {
        match token {
            "fn" => TokenKind::Keyword(Keyword::Function),
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "for" => TokenKind::Keyword(Keyword::For),
            "while" => TokenKind::Keyword(Keyword::While),
            "loop" => TokenKind::Keyword(Keyword::Loop),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "in" => TokenKind::Keyword(Keyword::In),
            "{" => TokenKind::Symbol(Symbol::OpenBracket),
            "}" => TokenKind::Symbol(Symbol::CloseBracket),
            "(" => TokenKind::Symbol(Symbol::OpenParen),
            ")" => TokenKind::Symbol(Symbol::CloseParen),
            "=" => TokenKind::Symbol(Symbol::Equal),
            "==" => TokenKind::Symbol(Symbol::DoubleEqual),
            ";" => TokenKind::Symbol(Symbol::SemiColon),
            "'" => TokenKind::Symbol(Symbol::SingleQuote),
            "\"" => TokenKind::Symbol(Symbol::DoubleQuote),
            "+" => TokenKind::Symbol(Symbol::Add),
            "-" => TokenKind::Symbol(Symbol::Sub),
            "*" => TokenKind::Symbol(Symbol::Mul),
            "/" => TokenKind::Symbol(Symbol::Div),
            "**" => TokenKind::Symbol(Symbol::Pow),
            "+=" => TokenKind::Symbol(Symbol::AddAssign),
            "-=" => TokenKind::Symbol(Symbol::SubAssign),
            ">" => TokenKind::Symbol(Symbol::Gt),
            "<" => TokenKind::Symbol(Symbol::Lt),
            ">=" => TokenKind::Symbol(Symbol::GtEq),
            "<=" => TokenKind::Symbol(Symbol::LtEq),
            ">>" => TokenKind::Symbol(Symbol::Shr),
            "<<" => TokenKind::Symbol(Symbol::Shl),
            "^" => TokenKind::Symbol(Symbol::Xor),
            "&" => TokenKind::Symbol(Symbol::BinaryAnd),
            "|" => TokenKind::Symbol(Symbol::BinaryOr),
            "&&" => TokenKind::Symbol(Symbol::LogicalAnd),
            "||" => TokenKind::Symbol(Symbol::LogicalOr),
            "true" => TokenKind::Literal(Literal::Bool(true)),
            "false" => TokenKind::Literal(Literal::Bool(false)),
            _ => {
                TokenKind::Identifier(token.to_owned())
            }
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Token {
    token_kind: TokenKind,
    pos: Pos
}

impl Token {
    pub fn new(token_kind: TokenKind, pos: Pos) -> Token {
        Token {
            token_kind,
            pos
        }
    }
}

macro_rules! double_identifier {
    ( $i:literal, $cur:ident, $idents:ident, $self:ident ) => {
        if $cur == $i {
            $idents.push(Token {
                    token_kind: TokenKind::new(concat!($i, $i)),
                    pos: $self.pos
                }
            );
            $cur = "";
            continue;
        }
        
        if $cur != "" {
            $idents.push(Token {
                    token_kind: TokenKind::new($cur),
                    pos: $self.pos
                });
        }

        $cur = $i;
    }
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer {
            pos: Pos::new()
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
            self.pos.col += 1;
            match literal {
                LiteralType::Str => {
                    if c == '"' {
                        tokens.push(Token {
                                token_kind: TokenKind::Literal(Literal::Str(current_identifier.to_owned())),
                                pos: self.pos
                            }
                        );
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
                            tokens.push(Token {
                                token_kind: TokenKind::Literal(Literal::Int(u64::from_str_radix(current_identifier, integer_base).unwrap())),
                                pos: self.pos
                            });
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
                        tokens.push(
                            Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            }
                        );
                        current_identifier = "";
                    }
                },
                '\n' => {
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                        current_identifier = "";
                    }
                    self.pos.row += 1;
                    self.pos.col = 0;
                }
                '{' | '}' | '(' | ')' | ';' | '^' => {
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                        current_identifier = "";
                    }
                    tokens.push(Token {
                            token_kind: TokenKind::new(&c.to_string()),
                            pos: self.pos
                        }
                    );
                },
                '+' => {
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                    }
                    current_identifier = "+";
                },
                '-' => {
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                    }
                    current_identifier = "-";
                },
                '"' => {
                    literal = LiteralType::Str;
                    continue;
                }
                '=' => {
                    if current_identifier == "=" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::DoubleEqual),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "+" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::AddAssign),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "-" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::SubAssign),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "*" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::MulAssign),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "/" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::DivAssign),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == "<" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::LtEq),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }

                    if current_identifier == ">" {
                        tokens.push(Token {
                                token_kind: TokenKind::Symbol(Symbol::GtEq),
                                pos: self.pos
                            });
                        current_identifier = "";
                        continue;
                    }


                    
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                    }

                    current_identifier = "=";
                }
                '&' => {
                    double_identifier!("&", current_identifier, tokens, self);
                }
                '|' => {
                    double_identifier!("|", current_identifier, tokens, self);
                }
                '*' => {
                    double_identifier!("*", current_identifier, tokens, self);
                }
                '/' => {
                    double_identifier!("/", current_identifier, tokens, self);
                }
                '>' => {
                    double_identifier!(">", current_identifier, tokens, self);
                }
                '<' => {
                    double_identifier!("<", current_identifier, tokens, self);
                }
                '0'..='9' => {
                    literal = LiteralType::Int;
                    if current_identifier != "" {
                        tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                        current_identifier = "";
                    }
                    ci = format!("{}{}", current_identifier, c);
                    current_identifier = ci.as_ref();
                }
                _ => {
                    match current_identifier {
                        "=" | "&" | "|" | "*" | "/" | "<" | ">" | "+" | "-" => {
                            tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
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
                tokens.push(Token {
                        token_kind: TokenKind::Literal(Literal::Int(u64::from_str_radix(current_identifier, integer_base).unwrap())),
                        pos: self.pos
                    }
                );
            }
            LiteralType::Str => {
                tokens.push(Token {
                        token_kind: TokenKind::Literal(Literal::Str(current_identifier.to_owned())),
                        pos: self.pos
                    });
            }
            _ => {
                if current_identifier != "" {
                    tokens.push(Token {
                                token_kind: TokenKind::new(current_identifier),
                                pos: self.pos
                            });
                }
            }
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenKind::*;
    use super::Literal::*;
    use super::Symbol::*;
    #[test]
    fn test() {
        let mut lexer = Lexer::new();
        
        assert_eq!(
            lexer.lex("fn hi(){if 1==1{print(\"hi\");}}").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("hi"), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 7 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Keyword(If), pos: Pos { row: 0, col: 11 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 13 } }, Token { token_kind: Symbol(DoubleEqual), pos: Pos { row: 0, col: 14 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 16 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 16 } }, Token { token_kind: Identifier("print"), pos: Pos { row: 0, col: 22 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 22 } }, Token { token_kind: Literal(Str("\\hi\\")), pos: Pos { row: 
0, col: 28 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 29 } }, Token { token_kind: Symbol(SemiColon), pos: Pos { row: 0, col: 30 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { row: 0, col: 31 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { row: 0, col: 32 } }])
        );
        assert_eq!(
            lexer.lex("fn hi() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("hi"), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 7 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 9 } }, Token { token_kind: Identifier("\\n\\tif"), pos: Pos { row: 0, col: 16 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 18 } }, Token { token_kind: Symbol(DoubleEqual), pos: Pos { row: 0, col: 20 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 23 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 24 } }, Token { token_kind: Identifier("\\n\\t\\tprint"), pos: Pos { row: 0, col: 36 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 36 } }, Token { token_kind: Literal(Str("\\hi\\")), pos: Pos { row: 0, col: 42 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 43 } }, Token { token_kind: Symbol(SemiColon), pos: Pos { row: 0, col: 44 } }, Token { token_kind: Identifier("\\n"), pos: Pos { row: 0, col: 47 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { 
row: 0, col: 47 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { row: 0, col: 48 } }])
        );
        assert_eq!(
            lexer.lex("/**/fn hi/**//**/() {\n\tif 1 == 1 {\n\t\tprint(\"hi\");\n}}// aje=  df d").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("hi"), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 7 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 9 } }, Token { token_kind: Identifier("\\n\\tif"), pos: Pos { row: 0, col: 16 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 18 } }, Token { token_kind: Symbol(DoubleEqual), pos: Pos { row: 0, col: 20 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 23 } }, Token { token_kind: Symbol(OpenBracket), pos: Pos { row: 0, col: 24 } }, Token { token_kind: Identifier("\\n\\t\\tprint"), pos: Pos { row: 0, col: 36 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 36 } }, Token { token_kind: Literal(Str("\\hi\\")), pos: Pos { row: 0, col: 42 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 43 } }, Token { token_kind: Symbol(SemiColon), pos: Pos { row: 0, col: 44 } }, Token { token_kind: Identifier("\\n"), pos: Pos { row: 0, col: 47 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { 
row: 0, col: 47 } }, Token { token_kind: Symbol(CloseBracket), pos: Pos { row: 0, col: 48 } }])
        );
        assert_eq!(
            lexer.lex("1+1").unwrap(),
            vec!([Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 2 } }, Token { token_kind: Symbol(Add), pos: Pos { row: 0, col: 4 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 5 } }])
        );
        assert_eq!(
            lexer.lex("1+1 ").unwrap(),
            vec!([Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 2 } }, Token { token_kind: Symbol(Add), pos: Pos { row: 0, col: 4 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 5 } }])
        );
        assert_eq!(
            lexer.lex("1 + 1").unwrap(),
            vec!([Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 2 } }, Token { token_kind: Symbol(Add), pos: Pos { row: 0, col: 4 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 5 } }])
        );
        assert_eq!(
            lexer.lex("let x = 1 + 1;").unwrap(),
            vec! ([Token { token_kind: Keyword(Let), pos: Pos { row: 0, col: 4 } }, Token { token_kind: Identifier("x"), pos: Pos { row: 0, col: 6 } }, Token { token_kind: Symbol(Equal), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 10 } }, Token { token_kind: Symbol(Add), pos: Pos { row: 0, col: 12 } }, Token { token_kind: Literal(Int(1)), pos: Pos { row: 0, col: 14 } }, Token { token_kind: Symbol(SemiColon), pos: Pos { row: 0, col: 14 } }])
        );
        assert_eq!(
            lexer.lex("\"hi\"").unwrap(),
            vec!([Token { token_kind: Literal(Str("\\hi\\")), pos: Pos { row: 0, col: 6 } }])
        );
        assert_eq!(
            lexer.lex("fn func()").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("func"), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 9 } }])
        );
         assert_eq!(
            lexer.lex("fn func ()").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("func"), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 9 } }])
        );
         assert_eq!(
            lexer.lex("fn func (  )").unwrap(),
            vec!([Token { token_kind: Keyword(Function), pos: Pos { row: 0, col: 3 } }, Token { token_kind: Identifier("func"), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(OpenParen), pos: Pos { row: 0, col: 8 } }, Token { token_kind: Symbol(CloseParen), pos: Pos { row: 0, col: 9 } }])
        );
        assert_eq!(
            lexer.lex("| |").unwrap(),
            vec!([Token { token_kind: Symbol(BinaryOr), pos: Pos { row: 0, col: 2 } }, Token { token_kind: Symbol(BinaryOr), pos: Pos { row: 0, col: 3 } }])
        );
        
      
    }
}