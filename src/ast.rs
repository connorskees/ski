use crate::lexer::{TokenKind, Literal};
use std::boxed::Box;

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
    Literal(Literal),
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
