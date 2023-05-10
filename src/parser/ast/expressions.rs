use super::operators::{BinaryOp, UnaryOp};
use super::literals::Literal;
use super::blocks::Block;

#[derive(Debug, Clone)]
pub enum ExprKind<'a> {
    Binary(BinaryOp, Expr<'a>, Expr<'a>),
    Unary(UnaryOp, Expr<'a>),
    Literal(Literal<'a>),
    Array(Vec<Expr<'a>>),
    StructInit(&'a str, Vec<Expr<'a>>),
    FieldAccess(Expr<'a>, &'a str),
    MethodCall(Expr<'a>, &'a str, Vec<Expr<'a>>),
    If(Expr<'a>, Block<'a>),
    While(Expr<'a>, Block<'a>),
    Assign(Expr<'a>, Expr<'a>),
    Call(&'a str, Vec<Expr<'a>>),
    Var(&'a str),
    Return(Option<Expr<'a>>),
}

impl <'a> Into<Expr<'a>> for ExprKind<'a> {
    fn into(self) -> Expr<'a> {
        Expr {
            kind: Box::new(self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr<'a> {
    pub kind: Box<ExprKind<'a>>,
}




