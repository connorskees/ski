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
                Keyword::Function => return self.eat_fn_decl(),
                Keyword::Let => return self.eat_var_decl(),
                Keyword::Const => return self.eat_const_decl(),
                Keyword::For => return self.eat_for(),
                Keyword::Num => unreachable!(),
                Keyword::In => unreachable!(),
                Keyword::While => return self.eat_while(),
                Keyword::Loop => return self.eat_loop(),
                Keyword::Return => return self.eat_return(),
                Keyword::If => return self.eat_if(),
                Keyword::Else => unreachable!(),
                Keyword::Continue => return self.eat_continue(),
                Keyword::Break => return self.eat_break(),
            }
        } else if let &TokenKind::Symbol(Symbol::OpenBracket) = &tok.token_kind {
            return self.eat_block();
        } else if let &TokenKind::Identifier(ref ident) = &tok.token_kind {
            let clone = ident.clone();
            match self.peek_token()?.token_kind {
                TokenKind::Symbol(Symbol::OpenParen) => return self.eat_fn_call(clone),
                TokenKind::Symbol(Symbol::AddAssign)
                | TokenKind::Symbol(Symbol::SubAssign)
                | TokenKind::Symbol(Symbol::MulAssign)
                | TokenKind::Symbol(Symbol::DivAssign) => return self.eat_mut_assign(clone),
                _ => {
                    return Err(ParseError::Error(
                        "unexpected token following identifier",
                        line!(),
                    ))
                }
            }
        }
        dbg!(&tok);
        unimplemented!()
    }

    fn eat_block(&mut self) -> PResult {
        let mut stmts: Vec<Expr> = Vec::new();
        loop {
            match self.peek_token()?.token_kind {
                TokenKind::Eof => return Err(ParseError::Eof),
                TokenKind::Symbol(Symbol::CloseBracket) => {
                    self.eat_token();
                    break;
                }
                _ => {
                    stmts.push(self.eat_stmt()?);
                }
            }
        }
        Ok(Expr::Block(stmts))
    }

    fn eat_if(&mut self) -> PResult {
        let cond = self.eat_expr()?;
        let then = self.eat_stmt()?;
        let else_: Expr = if expect_optional_keyword!(self, Else) {
            self.eat_stmt()?
        } else {
            Expr::Block(Vec::new())
        };
        Ok(Expr::If(Box::new(If { cond, then, else_ })))
    }

    fn eat_assign(&mut self) -> Result<(String, Expr, bool), ParseError> {
        let name = self.eat_ident()?;
        let is_numeric = expect_optional_symbol!(self, Colon);
        if is_numeric {
            expect_keyword!(self, Num, "expected keyword 'num'");
        }
        expect_symbol!(self, Assign, "expected '='");
        let value = self.eat_expr()?;
        expect_symbol!(self, SemiColon, "expected ';'");
        Ok((name, value, is_numeric))
    }

    fn eat_var_decl(&mut self) -> PResult {
        let (name, value, is_numeric) = self.eat_assign()?;
        Ok(Expr::VariableDecl(Box::new(VariableDecl { name, value, is_numeric})))
    }

    fn eat_const_decl(&mut self) -> PResult {
        let (name, value, is_numeric) = self.eat_assign()?;
        Ok(Expr::VariableDecl(Box::new(VariableDecl { name, value, is_numeric})))
    }

    fn eat_mut_assign(&mut self, name: String) -> PResult {
        let op = match self.eat_token().token_kind {
            TokenKind::Symbol(Symbol::AddAssign) => BinaryOpKind::Add,
            TokenKind::Symbol(Symbol::SubAssign) => BinaryOpKind::Sub,
            TokenKind::Symbol(Symbol::MulAssign) => BinaryOpKind::Mul,
            TokenKind::Symbol(Symbol::DivAssign) => BinaryOpKind::Div,
            _ => unreachable!(),
        };
        let right = self.eat_expr()?;
        expect_symbol!(self, SemiColon, "expected ';'");
        Ok(Expr::Binary(Box::new(BinaryExpr {
            left: Expr::Variable(name),
            op,
            right,
        })))
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
                    _ => return Err(ParseError::Error("expected ',' or ')'", line!())),
                };
            }
        } else {
            expect_symbol!(self, CloseParen, "expected symbol ')'");
        }
        let body = self.eat_stmt()?;
        Ok(Expr::FuncDef(Box::new(FuncDef { name, params, body })))
    }

    fn eat_fn_call(&mut self, func_name: String) -> PResult {
        expect_symbol!(self, OpenParen, "expected '('");
        let mut params: Vec<Expr> = Vec::new();
        loop {
            let tok = self.eat_expr()?;
            params.push(tok);
            match self.eat_token().token_kind {
                TokenKind::Symbol(Symbol::Comma) => continue,
                TokenKind::Symbol(Symbol::CloseParen) => break,
                _ => return Err(ParseError::Error("expected ',' or ')'", line!())),
            }
        }
        expect_optional_symbol!(self, SemiColon);
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
        let expr = self.eat_expr()?;
        expect_symbol!(self, SemiColon, "expected ';'");
        Ok(Expr::Return(Box::new(expr)))
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
        let has_open_paren = expect_optional_symbol!(self, OpenParen);
        match self.peek_token()?.token_kind {
            TokenKind::Symbol(Symbol::Sub)
            | TokenKind::Symbol(Symbol::LogicalNot)
            | TokenKind::Symbol(Symbol::BitwiseNot) => {
                return self.eat_unary();
            }
            TokenKind::Eof => return Err(ParseError::Eof),
            _ => {}
        }

        macro_rules! bin_op {
            ($self:ident, $left:ident, $open_paren:ident, $( $type:ident ),*) => {
                match $self.peek_token()?.token_kind {
                    $(TokenKind::Symbol(Symbol::$type) => BinaryOpKind::$type,)*
                    TokenKind::Symbol(Symbol::CloseParen) => {
                        if $open_paren {
                            $self.eat_token();
                        }
                        return Ok($left)
                    },
                    TokenKind::Eof => return Err(ParseError::Eof),
                    TokenKind::Literal(_) => return Ok(self.eat_literal()?),
                    _ => return Ok($left)
                }
            }
        };

        let left = self.eat_var_or_literal()?;
        let op = bin_op!(
            self,
            left,
            has_open_paren,
            Add,
            Sub,
            Mul,
            Div,
            Assign,
            Eq,
            Ne,
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
            BinaryOr
        );
        self.eat_token();
        let right = self.eat_expr()?;
        if has_open_paren {
            expect_symbol!(self, CloseParen, "expected ')'");
        }
        Ok(Expr::Binary(Box::new(BinaryExpr { left, op, right })))
    }

    fn eat_unary(&mut self) -> PResult {
        let op = match self.eat_token().token_kind {
            TokenKind::Symbol(Symbol::Sub) => UnaryOpKind::Minus,
            TokenKind::Symbol(Symbol::LogicalNot) => UnaryOpKind::LogicalNot,
            TokenKind::Symbol(Symbol::BitwiseNot) => UnaryOpKind::BitwiseNot,
            _ => unreachable!(),
        };

        let child = match self.peek_token()?.token_kind {
            TokenKind::Symbol(Symbol::OpenParen) => self.eat_expr()?,
            _ => self.eat_var_or_literal()?,
        };

        macro_rules! is_op_next {
            ($self:ident, $( $type:ident ),*) => {
                #[allow(unreachable_patterns)]
                match $self.peek_token()?.token_kind {
                    $(TokenKind::Symbol(Symbol::$type) | )* TokenKind::Symbol(Symbol::Add) => {
                        return Ok(
                            Expr::Binary(
                                Box::new(
                                    BinaryExpr {
                                        left: Expr::Unary(Box::new(UnaryExpr { op, child })),
                                        op: BinaryOpKind::from_token(&self.eat_token().token_kind)?,
                                        right: self.eat_expr()?,
                                    }
                                )
                            )
                        )
                    },
                    _ => {}
                }
            }
        };

        is_op_next!(
            self, Add, Sub, Mul, Div, Assign, Eq, Ne, Gt, Lt, GtEq, LtEq, Shr, Shl, Xor,
            LogicalAnd, LogicalOr, BinaryAnd, BinaryOr
        );
        Ok(Expr::Unary(Box::new(UnaryExpr { op, child })))
    }

    fn eat_literal(&mut self) -> PResult {
        match self.eat_token().token_kind {
            TokenKind::Literal(Literal::Str(ref s)) => Ok(Expr::Str(s.to_string())),
            TokenKind::Literal(Literal::Int(i)) => Ok(Expr::Int(i)),
            _ => Err(ParseError::Error("expected literal", line!())),
        }
    }

    fn eat_ident(&mut self) -> Result<String, ParseError> {
        match self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => Ok(ident.to_string()),
            _ => Err(ParseError::Error("expected identifier", line!())),
        }
    }

    fn eat_var_or_literal(&mut self) -> PResult {
        match self.eat_token().token_kind {
            TokenKind::Identifier(ref ident) => Ok(Expr::Variable(ident.to_string())),
            TokenKind::Literal(Literal::Str(ref s)) => Ok(Expr::Str(s.to_string())),
            TokenKind::Literal(Literal::Int(i)) => Ok(Expr::Int(i)),
            _ => Err(ParseError::Error("expected identifier or literal", line!())),
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
