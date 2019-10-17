use crate::lexer::Literal;
use std::boxed::Box;
use std::fmt;

pub struct AST {

}
/*
let x = 1 + 1
*/

// Expr::VariableDecl (
//     VariableDecl {
//         name: "x",
//         value: Expr::Binary(
//             Box<BinaryExpr{
//                 op: Add,
//                 left: Expr::Literal(Literal(Int(1))),
//                 right: Expr::Literal(Literal(Int(1))),
//             }>
//         )
//     }
// )

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Expr {
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Literal(Literal),
    Return(Option<Box<Expr>>),
    VariableDecl(Box<VariableDecl>),
    If(Box<If>),
    FuncDef(Box<FuncDef>),
    FuncCall(Box<FuncCall>),
    While(Box<While>),
    Loop(Box<Expr>),
    For(Box<For>),
    Block(Vec<Expr>),
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct UnaryExpr {
    pub op: UnaryOpKind,
    pub child: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct BinaryExpr {
    pub op: BinaryOpKind,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct FuncDef {
    pub name: String,
    pub param_names: Vec<String>,
    pub body: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct FuncCall {
    pub func: Expr,
    pub params: Vec<Expr>
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct If {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Expr
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct For {
    pub item: String,
    pub container: Expr
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct While {
    pub cond: Expr,
    pub body: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
struct VariableDecl {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum UnaryOpKind {
    Minus,
    Negate
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
