use std::boxed::Box;
use std::fmt;
use std::fmt::Write;

use crate::lexer::{Symbol, TokenKind};

/*
let x = 1 + 1
*/

// Expr::VariableDecl (
//     VariableDecl {
//         name: "x",
//         value: Expr::Binary(
//             Box<BinaryExpr{
//                 op: Add,
//                 left: Expr::(Int(1),
//                 right: Expr::(Int(1),
//             }>
//         )
//     }
// )

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Expr {
    Int(u64),
    Str(String),
    Variable(String),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Return(Box<Expr>),
    VariableDecl(Box<VariableDecl>),
    ConstDecl(Box<ConstDecl>),
    If(Box<If>),
    FuncDef(Box<FuncDef>),
    FuncCall(Box<FuncCall>),
    While(Box<While>),
    Loop(Box<Loop>),
    For(Box<For>),
    Continue,
    Break,
    Block(Vec<Expr>),
}


#[derive(Debug, Hash, Eq, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOpKind,
    pub child: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOpKind,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FuncCall {
    pub func_name: String,
    pub params: Vec<Expr>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct If {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct For {
    pub item: String,
    pub container: Expr,
    pub body: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct While {
    pub cond: Expr,

    pub body: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct Loop {
    pub body: Expr,
}


#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expr,
    //TODO: pub is_numeric: bool
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum UnaryOpKind {
    Minus,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum BinaryOpKind {
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
    BinaryOr,
}

impl BinaryOpKind {
    pub fn from_token(t: &TokenKind) -> Result<BinaryOpKind, &'static str> {
        if let TokenKind::Symbol(ref sym) = t {
            match *sym {
                Symbol::Add => Ok(BinaryOpKind::Add),
                Symbol::Sub => Ok(BinaryOpKind::Sub),
                Symbol::Mul => Ok(BinaryOpKind::Mul),
                Symbol::Div => Ok(BinaryOpKind::Div),
                Symbol::Assign => Ok(BinaryOpKind::Assign),
                Symbol::Eq => Ok(BinaryOpKind::Eq),
                Symbol::Ne => Ok(BinaryOpKind::Ne),
                Symbol::Gt => Ok(BinaryOpKind::Gt),
                Symbol::Lt => Ok(BinaryOpKind::Lt),
                Symbol::GtEq => Ok(BinaryOpKind::GtEq),
                Symbol::LtEq => Ok(BinaryOpKind::LtEq),
                Symbol::Shr => Ok(BinaryOpKind::Shr),
                Symbol::Shl => Ok(BinaryOpKind::Shl),
                Symbol::Xor => Ok(BinaryOpKind::Xor),
                Symbol::LogicalAnd => Ok(BinaryOpKind::LogicalAnd),
                Symbol::LogicalOr => Ok(BinaryOpKind::LogicalOr),
                Symbol::BinaryAnd => Ok(BinaryOpKind::BinaryAnd),
                Symbol::BinaryOr => Ok(BinaryOpKind::BinaryOr),
                _ => Err("unexpected symbol type"),
            }
        } else {
            Err("unexpected token type")
        }
    }
}
