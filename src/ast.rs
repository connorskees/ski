use crate::lexer::{Literal, Symbol, TokenKind};
use std::boxed::Box;
use std::fmt;
// use std::io::Write;
use std::fmt::Write;

pub trait Compile {
    fn compile(&self) -> String;
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

impl Compile for Vec<Expr> {
    fn compile(&self) -> String {
        self.iter().map(|s| s.compile()).collect::<Vec<String>>().join("\n")
    }
}

impl Compile for Expr {
    fn compile(&self) -> String {
        use Expr::*;
        match self {
            Int(_) => unimplemented!(),
            Str(_) => unimplemented!(),
            Variable(_) => unimplemented!(),
            Unary(_) => unimplemented!(),
            Binary(_) => unimplemented!(),
            Literal(_) => unimplemented!(),
            Return(_) => unimplemented!(),
            VariableDecl(_) => unimplemented!(),
            ConstDecl(_) => unimplemented!(),
            If(_) => unimplemented!(),
            FuncDef(_) => unimplemented!(),
            FuncCall(c) => c.compile(),
            While(_) => unimplemented!(),
            Loop(_) => unimplemented!(),
            For(f) => f.compile(),
            Continue => unimplemented!(),
            Break => unimplemented!(),
            Block(b) => b.compile(),
        }
        // FOR %%item IN (set) DO command
    }
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

impl Compile for FuncCall {
    fn compile(&self) -> String {
        format!("(1, 2, 3, 4)")
        // format!("GOTO :{}\n{}", self.func_name, self.params.join("SET /A x = "))
    }
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

/// FOR %%item IN (set) DO command
impl Compile for For {
    fn compile(&self) -> String {
        format!("FOR %%{} IN {} DO {}", self.item, self.container.compile(), self.body.compile())
    }
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

impl Compile for Loop {
    fn compile(&self) -> String {
        format!(":LOOP\n{}\ngoto :LOOP", self.body.compile())
    }
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

impl BinaryOpKind {
    pub fn from_token(t: &TokenKind) -> Result<BinaryOpKind, &'static str> {
        if let TokenKind::Symbol(ref sym) = t {
            return match *sym {
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
            };
        } else {
            Err("unexpected token type")
        }
    }
}
