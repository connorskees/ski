use std::boxed::Box;

use crate::lexer::{Token, TokenKind, Symbol, Keyword, Pos, Literal};
use crate::ast::*;
use crate::errors::ParseError;

type PResult = Result<Expr, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

macro_rules! expect_keyword {
    ($self:ident, $keyword:ident, $err:literal) => {
        $self.expect_token(&TokenKind::Keyword(Keyword::$keyword), $err)?;
    }
}

macro_rules! expect_symbol {
    ($self:ident, $symbol:ident, $err:literal) => {
        $self.expect_token(&TokenKind::Symbol(Symbol::$symbol), $err)?;
    }
}

macro_rules! eat_literal {
    ($self:ident) => {
        match $self.eat_token().token_kind {
            TokenKind::Literal(Literal::Str(ref s)) => Expr::Str(s.to_string()),
            TokenKind::Literal(Literal::Int(i)) => Expr::Int(i),
            _ => unreachable!()
        };
    }
}

macro_rules! eat_ident { 
    ($self:ident) => {
        match $self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => ident.to_string(),
            _ => unreachable!()
        }
        
    }
}


impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens, cursor: 0usize
        }
    }

    pub fn parse(&mut self) -> PResult {
        self.eat_stmt()
    }

    fn eat_stmt(&mut self) -> PResult {
        let tok = self.eat_token();
        println!("{:?}", &tok);
        if let &TokenKind::Keyword(ref keyw) = &tok.token_kind {
            match *keyw {
                Keyword::If => return self.eat_if(),
                Keyword::For => return self.eat_for(),
                Keyword::While => return self.eat_while(),
                Keyword::Loop => return self.eat_loop(),
                Keyword::Continue => return self.eat_continue(),
                Keyword::Break => return self.eat_break(),
                Keyword::Return => return self.eat_return(),
                Keyword::Let => return self.eat_var_decl(),
                Keyword::Function => return self.eat_fn_decl(),
                _ => {}
            }
        
        } else if let &TokenKind::Symbol(Symbol::OpenBracket) = &tok.token_kind {
            return self.eat_compound_stmt();
        
        } else if let &TokenKind::Identifier(ref ident) = &tok.token_kind {
            let clone = ident.clone();
            let next_token = self.eat_token();
            match next_token.token_kind {
                TokenKind::Symbol(Symbol::OpenParen) => return self.eat_fn_call(clone),
                _ => {
                    println!("{:?}", &next_token);
                    unimplemented!()
                },
            }

           
        }
        unimplemented!()
    }

    fn eat_compound_stmt(&mut self) -> PResult {
        let mut stmts: Vec<Expr> = Vec::new();
        loop {
            match self.peek_token().unwrap().token_kind {
                TokenKind::Eof => return Err(ParseError::Eof),
                TokenKind::Symbol(Symbol::CloseBracket) => break,
                _ => {
                    stmts.push(self.eat_stmt()?);
                }
            }
        }
        Ok(Expr::Block(stmts))
    }

    fn eat_if(&mut self) -> PResult {
        unimplemented!()
    }
    
    fn eat_var_decl(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_fn_decl(&mut self) -> PResult {
        let mut params: Vec<String> = Vec::new();
        let func_name = eat_ident!(self);
        expect_symbol!(self, OpenParen, "expected symbol '('");
        let tok = self.eat_token();
        if tok.token_kind != TokenKind::Symbol(Symbol::CloseParen) {
            loop {
                match self.eat_token().token_kind {
                    TokenKind::Symbol(Symbol::Comma) => continue,
                    TokenKind::Symbol(Symbol::CloseParen) => break,
                    _ => unimplemented!()
                };
                params.push(eat_ident!(self));
            }
        }
        let body = self.eat_stmt()?;
        Ok(Expr::FuncDef(
                Box::new(FuncDef {
                    name: func_name, params, body
                }), 
            )
        )
    }

    fn eat_fn_call(&mut self, func_name: String) -> PResult {
        let mut params: Vec<Expr> = Vec::new();
        loop { 
            let tok = eat_literal!(self);
            params.push(tok);
            match self.eat_token().token_kind {
                TokenKind::Symbol(Symbol::Comma) => continue,
                TokenKind::Symbol(Symbol::CloseParen) => break,
                _ => unimplemented!()
            }    
        }
         Ok(Expr::FuncCall(
                Box::new(FuncCall {
                    func_name, params
                }), 
            )
        )
        
    }

    fn eat_fn_params(&mut self) -> Vec<String> {
        unimplemented!()
    }

    fn eat_while(&mut self) -> PResult {
        expect_keyword!(self, While, "expected keyword 'while'");
        let cond = unimplemented!();
        let body = self.eat_stmt();
        
    }

    fn eat_loop(&mut self) -> PResult {
        let body = self.eat_stmt()?;
         Ok(Expr::Loop(
                Box::new(Loop {
                    body
                }), 
            )
        )
    }

    fn eat_continue(&mut self) -> PResult {
        expect_symbol!(self, SemiColon, "expected symbol ';'");
        Ok(Expr::Continue)
    }

    fn eat_break(&mut self) -> PResult {
         expect_symbol!(self, SemiColon, "expected symbol ';'");
        Ok(Expr::Break)
    }

    fn eat_return(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_for(&mut self) -> PResult {
        let item = eat_ident!(self);
        expect_keyword!(self, In, "expected keyword 'in'");
        let container = self.eat_stmt()?;
        let body = self.eat_stmt()?;
        Ok(Expr::For(
                Box::new(For {
                    item, container, body
                }), 
            )
        )
    }

    fn expect_token(&mut self, t: &TokenKind, msg: &'static str) -> Result<(), &'static str> {
        if t != &self.tokens[self.cursor].token_kind {
            Err(msg)
        } else {
            self.cursor += 1;
            Ok(())
        }
    }

    fn eat_token(&mut self) -> &Token {
        self.cursor += 1;
        &self.tokens[self.cursor-1]
    }

    fn peek_token(&mut self) -> Option<&Token> {
        if &self.tokens.len() <= &(self.cursor) {
            None
        } else {
            Some(&self.tokens[self.cursor])
        }
    }

    fn ident_val(t: TokenKind) -> String {
        match t {
            _ => unreachable!()
        }
    }
}