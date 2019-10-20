use std::boxed::Box;

use crate::ast::*;
use crate::errors::ParseError;
use crate::lexer::{Keyword, Literal, Pos, Symbol, Token, TokenKind};

type PResult = Result<Expr, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

macro_rules! expect_keyword {
    ($self:ident, $keyword:ident, $err:literal) => {
        $self.expect_token(&TokenKind::Keyword(Keyword::$keyword), $err)?;
    };
}

macro_rules! expect_optional_keyword {
    ($self:ident, $keyword:ident) => {
        $self.expect_optional_token(&TokenKind::Keyword(Keyword::$keyword));
    };
}

macro_rules! expect_optional_symbol {
    ($self:ident, $symbol:ident) => {
        $self.expect_optional_token(&TokenKind::Symbol(Symbol::$symbol));
    };
}

macro_rules! expect_symbol {
    ($self:ident, $symbol:ident, $err:literal) => {
        $self.expect_token(&TokenKind::Symbol(Symbol::$symbol), $err)?;
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            cursor: 0usize,
        }
    }

    pub fn parse(&mut self) -> PResult {
        self.eat_stmt()
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
                }
            }
        }
        dbg!(&tok);
        unimplemented!()
    }

    fn eat_compound_stmt(&mut self) -> PResult {
        let mut stmts: Vec<Expr> = Vec::new();
        loop {
            match self.peek_token()?.token_kind {
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
        let cond = self.eat_expr()?;
        dbg!(&cond);
        let then = self.eat_stmt()?;
        dbg!(&then);
        let else_: Expr = if expect_optional_keyword!(self, Else) {
            self.eat_stmt()?
        } else {
            Expr::Block(Vec::new())
        };
        Ok(Expr::If(Box::new(If { cond, then, else_ })))
    }

    fn eat_var_decl(&mut self) -> PResult {
        unimplemented!()
    }

    fn eat_fn_decl(&mut self) -> PResult {
        let mut params: Vec<String> = Vec::new();
        let name = self.eat_ident()?;
        expect_symbol!(self, OpenParen, "expected symbol '('");
        if let TokenKind::Identifier(_) = self.peek_token()?.token_kind {
            loop {
                params.push(self.eat_ident()?);
                match self.eat_token().token_kind {
                    TokenKind::Symbol(Symbol::Comma) => continue,
                    TokenKind::Symbol(Symbol::CloseParen) => break,
                    _ => return Err(ParseError::Error("expected ',' or ')'")),
                };
            }
        } else {
            expect_symbol!(self, CloseParen, "expected symbol ')'");
        }
        let body = self.eat_stmt()?;
        Ok(Expr::FuncDef(Box::new(FuncDef { name, params, body })))
    }

    fn eat_fn_call(&mut self, func_name: String) -> PResult {
        let mut params: Vec<Expr> = Vec::new();
        loop {
            // TODO: let tok = self.eat_expr();
            let tok = self.eat_literal()?;
            params.push(tok);
            match self.eat_token().token_kind {
                TokenKind::Symbol(Symbol::Comma) => continue,
                TokenKind::Symbol(Symbol::CloseParen) => break,
                _ => return Err(ParseError::Error("expected ',' or ')'")),
            }
        }
        Ok(Expr::FuncCall(Box::new(FuncCall { func_name, params })))
    }

    fn eat_while(&mut self) -> PResult {
        let cond = self.eat_expr()?;
        let body = self.eat_stmt()?;
        Ok(Expr::While(Box::new(While { cond, body })))
    }

    fn eat_loop(&mut self) -> PResult {
        let body = self.eat_stmt()?;
        Ok(Expr::Loop(Box::new(Loop { body })))
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
        let item = self.eat_ident()?;
        expect_keyword!(self, In, "expected keyword 'in'");
        let container = self.eat_stmt()?;
        let body = self.eat_stmt()?;
        Ok(Expr::For(Box::new(For {
            item,
            container,
            body,
        })))
    }

    fn eat_expr(&mut self) -> PResult {
        expect_optional_symbol!(self, OpenParen);
        match self.peek_token()?.token_kind {
            TokenKind::Symbol(Symbol::Sub)
            | TokenKind::Symbol(Symbol::LogicalNot)
            | TokenKind::Symbol(Symbol::BitwiseNot) => {
                return self.eat_unary();
            },
            TokenKind::Eof => return Err(ParseError::Eof),
            _ => {}
        }

        macro_rules! bin_op {
            ($self:ident, $left:ident, $( $type:ident ),*) => {
                match $self.peek_token()?.token_kind {
                    $(TokenKind::Symbol(Symbol::$type) => BinaryOpKind::$type,)*
                    TokenKind::Symbol(Symbol::CloseParen) => {
                        $self.eat_token();
                        return Ok($left)
                    },
                    TokenKind::Eof => return Err(ParseError::Eof),
                    _ => return Ok($left)
                }
            }
        };

        let left = self.eat_var_or_literal()?;
        let op = bin_op!(
            self, left, Add, Sub, Mul, Div, Assign, Eq, Ne, Gt, Lt, GtEq, LtEq, Shr, Shl, Xor,
            LogicalAnd, LogicalOr, BinaryAnd, BinaryOr
        );
        self.eat_token();
        let right = self.eat_expr()?;
        expect_optional_symbol!(self, CloseParen);
        Ok(Expr::Binary(Box::new(BinaryExpr { left, op, right })))
    }

    fn eat_binary(&mut self) -> PResult {
        let left = self.eat_literal()?;
        let op = match self.eat_token().token_kind {
            TokenKind::Symbol(Symbol::Add) => BinaryOpKind::Add,
            _ => unimplemented!(),
        };
        let right = self.eat_literal()?;
        Ok(Expr::Binary(Box::new(BinaryExpr { left, op, right })))
    }

    fn eat_unary(&mut self) -> PResult {
        let op = match self.eat_token().token_kind {
            TokenKind::Symbol(Symbol::Sub) => UnaryOpKind::Minus,
            TokenKind::Symbol(Symbol::LogicalNot) => UnaryOpKind::LogicalNot,
            TokenKind::Symbol(Symbol::BitwiseNot) => UnaryOpKind::BitwiseNot,
            _ => unimplemented!(),
        };
        dbg!(&op);
        let child = match self.peek_token()?.token_kind {
            TokenKind::Symbol(Symbol::OpenParen) => self.eat_expr()?,
            _ => self.eat_var_or_literal()?,
        };
        dbg!(&child);
        Ok(Expr::Unary(Box::new(UnaryExpr { op, child })))
    }

    fn eat_literal(&mut self) -> PResult {
        match self.eat_token().token_kind {
            TokenKind::Literal(Literal::Str(ref s)) => Ok(Expr::Str(s.to_string())),
            TokenKind::Literal(Literal::Int(i)) => Ok(Expr::Int(i)),
            _ => Err(ParseError::Error("expected literal")),
        }
    }

    fn eat_ident(&mut self) -> Result<String, ParseError> {
        match self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => Ok(ident.to_string()),
            _ => Err(ParseError::Error("expected identifier")),
        }
    }

    fn eat_var_or_literal(&mut self) -> PResult {
        match self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => Ok(Expr::Variable(ident.to_string())),
            TokenKind::Literal(Literal::Str(ref s)) => Ok(Expr::Str(s.to_string())),
            TokenKind::Literal(Literal::Int(i)) => Ok(Expr::Int(i)),
            _ => Err(ParseError::Error("expected identifier or literal")),
        }
    }

    fn expect_token(&mut self, t: &TokenKind, msg: &'static str) -> Result<(), &'static str> {
        if t != &self.tokens[self.cursor].token_kind {
            Err(msg)
        } else {
            self.cursor += 1;
            Ok(())
        }
    }

    fn expect_optional_token(&mut self, t: &TokenKind) -> bool {
        if t != &self.tokens[self.cursor].token_kind {
            false
        } else {
            self.cursor += 1;
            true
        }
    }

    fn eat_token(&mut self) -> &Token {
        self.cursor += 1;
        &self.tokens[self.cursor - 1]
    }

    fn peek_token(&mut self) -> Result<&Token, ParseError> {
        if &self.tokens.len() <= &(self.cursor) {
            Err(ParseError::Eof)
        } else {
            Ok(&self.tokens[self.cursor])
        }
    }

    fn ident_val(t: TokenKind) -> String {
        match t {
            _ => unreachable!(),
        }
    }
}
