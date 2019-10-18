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

macro_rules! ident_val {
    ($self:ident) => {
        match $self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => ident.to_string(),
            _ => {
                println!("{:?}", $self.eat_token().token_kind);
                unreachable!()
            }
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
        self.eat_for()
    }

    fn eat_stmt(&mut self) -> PResult {
        let tok = self.eat_token();
        if let &TokenKind::Keyword(ref keyw) = &tok.token_kind {
            match *keyw {
                Keyword::If => return self.eat_if(),
                Keyword::For => return self.eat_for(),
                Keyword::While => return self.eat_while(),
                Keyword::Loop => return self.eat_loop(),
                Keyword::Continue => return self.eat_continue(),
                Keyword::Break => return self.eat_break(),
                Keyword::Return => return self.eat_return(),
                _ => {}
            }
        }
        unimplemented!()
    }

    fn eat_if(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_while(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_loop(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_continue(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_break(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_return(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_for(&mut self) -> PResult {
        expect_keyword!(self, For, "expected keyword 'for'");
        let item = ident_val!(self);//Self::ident_val(self.eat_token().token_kind);
        expect_keyword!(self, In, "expected keyword 'in'");
        let container = eat_literal!(self);
        expect_symbol!(self, OpenBracket, "expected '{'");
        let body = self.eat_stmt()?;
        expect_symbol!(self, CloseBracket, "expected '}'");
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
        &self.tokens[self.cursor]
    }

    fn peek_token(&mut self) -> &Token {
        &self.tokens[self.cursor+1]
    }

    fn ident_val(t: TokenKind) -> String {
        match t {
            _ => unreachable!()
        }
    }
}