use super::operators::{BinaryOp, UnaryOp};
use super::literals::Literal;
use super::blocks::Block;

pub enum ExprKind<'a> {
    Binary(BinaryOp, Expr<'a>, Expr<'a>),
    Unary(UnaryOp, Expr<'a>),
    Literal(Literal<'a>),
    If(Expr<'a>, Block<'a>),
    While(Expr<'a>, Block<'a>),
    Assign(&'a str, Expr<'a>),
    Call(&'a str, Vec<Expr<'a>>),
    Var(&'a str),
}


pub struct Expr<'a> {
    pub kind: Box<ExprKind<'a>>,
}




