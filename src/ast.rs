use std::boxed::Box;
use std::fmt;
use std::fmt::Write;

use crate::compiler::{Compile, Target};
use crate::lexer::{Literal, Symbol, TokenKind};

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
    fn compile(&self, t: &Target) -> String {
        self.iter().map(|s| s.compile(t)).collect::<Vec<String>>().join("\n")
    }
    fn compile_asm(&self) -> String {
        unimplemented!()
    }
    fn compile_dos(&self) -> String {
        unimplemented!()
    }
    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

impl Compile for Expr {
    fn compile(&self, t: &Target) -> String {
        use Expr::*;
        match self {
            Int(i) => i.to_string(),
            Str(s) => s.to_owned(),
            Variable(v) => format!("%{}%", v),
            Unary(u) => u.compile(t),
            Binary(b) => b.compile(t),
            Literal(l) => l.compile(t),
            Return(_) => unimplemented!(),
            VariableDecl(v) => v.compile(t),
            ConstDecl(_) => unimplemented!(),
            If(f) => f.compile(t),
            FuncDef(_) => unimplemented!(),
            FuncCall(c) => c.compile(t),
            While(_) => unimplemented!(),
            Loop(_) => unimplemented!(),
            For(f) => f.compile(t),
            Continue => unimplemented!(),
            Break => unimplemented!(),
            Block(b) => b.compile(t),
        }
    }
    fn compile_asm(&self) -> String {
        unimplemented!()
    }
    fn compile_dos(&self) -> String {
        unimplemented!()
    }
    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOpKind,
    pub child: Expr,
}

impl Compile for Literal {
    fn compile(&self, t: &Target) -> String {
        unimplemented!()
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}
impl Compile for UnaryExpr {
    fn compile(&self, t: &Target) -> String {
        format!("{}{}", self.op.compile(t), self.child.compile(t))
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOpKind,
    pub left: Expr,
    pub right: Expr,
}

impl Compile for BinaryExpr {
    fn compile(&self, t: &Target) -> String {
        format!("{} {} {}", self.left.compile(t), self.op.compile(t), self.right.compile(t))
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: Expr,
}

impl Compile for FuncDef {
    fn compile(&self, t: &Target) -> String {
        unimplemented!()
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct FuncCall {
    pub func_name: String,
    pub params: Vec<Expr>,
}

impl Compile for FuncCall {
    fn compile(&self, t: &Target) -> String {
        format!("(1, 2, 3, 4)")
        // format!("GOTO :{}\n{}", self.func_name, self.params.join("SET /A x = "))
    }
    fn compile_asm(&self) -> String {
        unimplemented!()
    }
    fn compile_dos(&self) -> String {
        unimplemented!()
    }
    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct If {
    pub cond: Expr,
    pub then: Expr,
    pub else_: Expr,
}

impl Compile for If {
    fn compile(&self, t: &Target) -> String {
        format!("IF {} (\n{})\nELSE (\n{}\n)\n",
        self.cond.compile(t), self.then.compile(t), self.else_.compile(t))
    }
    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}


#[derive(Debug, Hash, Eq, PartialEq)]
pub struct For {
    pub item: String,
    pub container: Expr,
    pub body: Expr,
}

/// FOR %%item IN (set) DO command
impl Compile for For {
    fn compile(&self, t: &Target) -> String {
        format!("FOR %%{} IN {} DO (\n{})\n", self.item, self.container.compile(t), self.body.compile(t))
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
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
    fn compile(&self, t: &Target) -> String {
        format!(":LOOP\n{}\ngoto :LOOP", self.body.compile(t))
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expr,
    //TODO: pub is_numeric: bool
}

impl Compile for VariableDecl {
    fn compile(&self, t: &Target) -> String {
        format!("set {}={}\n", self.name, self.value.compile(t))
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub value: Expr,
}

impl Compile for ConstDecl {
    fn compile(&self, t: &Target) -> String {
        unimplemented!()
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum UnaryOpKind {
    Minus,
    LogicalNot,
    BitwiseNot,
}

impl Compile for UnaryOpKind {
    fn compile(&self, t: &Target) -> String {
        match self {
            UnaryOpKind::Minus => String::from("-"),
            UnaryOpKind::LogicalNot => String::from("NOT"),
            UnaryOpKind::BitwiseNot => String::from("~"),
        }
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
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
impl Compile for BinaryOpKind {
     fn compile(&self, t: &Target) -> String {
        match self {
            BinaryOpKind::Add => String::from("+"),
            BinaryOpKind::Sub => String::from("-"),
            BinaryOpKind::Mul => String::from("*"),
            BinaryOpKind::Div => String::from("/"),
            BinaryOpKind::Assign => String::from("EQU"),
            BinaryOpKind::Eq => String::from("=="),
            BinaryOpKind::Ne => String::from("NEQ"),
            BinaryOpKind::Gt => String::from("GTR"),
            BinaryOpKind::Lt => String::from("LSS"),
            BinaryOpKind::GtEq => String::from("GEQ"),
            BinaryOpKind::LtEq => String::from("LEQ"),
            BinaryOpKind::Shr => String::from(">>"),
            BinaryOpKind::Shl => String::from("<<"),
            BinaryOpKind::Xor => String::from("^"),
            BinaryOpKind::LogicalAnd => String::from("&&"),
            BinaryOpKind::LogicalOr => String::from("||"),
            BinaryOpKind::BinaryAnd => String::from("&"),
            BinaryOpKind::BinaryOr => String::from("|"),
        }
    }

    fn compile_asm(&self) -> String {
        unimplemented!()
    }

    fn compile_dos(&self) -> String {
        unimplemented!()
    }

    fn compile_bash(&self) -> String {
        unimplemented!()
    }
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
