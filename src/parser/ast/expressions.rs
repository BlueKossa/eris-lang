use crate::span::Span;

use super::blocks::Block;
use super::literals::Literal;
use super::operators::{BinaryOp, UnaryOp};
use super::types::Type;

#[derive(Debug, Clone)]
pub enum ExprKind {
    Binary(BinaryOp, Expr, Expr),
    Unary(UnaryOp, Expr),
    Literal(Literal),
    Address(Expr),
    Deref(Expr),
    Cast(Expr, Type),
    Array(Vec<Expr>),
    StructInit(Span, Vec<Expr>),
    FieldAccess(Expr, Span),
    ArrayIndex(Expr, Expr),
    Call(Span, Vec<Expr>),
    MethodCall(Expr, Span, Vec<Expr>),
    If(Expr, Block),
    Loop(Expr, Block),
    Assign(Expr, Expr),
    Var(Span),
    Break,
    Return(Option<Expr>),
}

impl From<ExprKind> for Expr {
    fn from(val: ExprKind) -> Self {
        Expr {
            kind: Box::new(val),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: Box<ExprKind>,
}

//impl std::fmt::Display for Expr {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match &*self.kind {
//            ExprKind::Binary(op, lhs, rhs) => {
//                write!(f, "({} {} {})", lhs, op, rhs)
//            }
//            ExprKind::Unary(op, expr) => {
//                write!(f, "({}{})", op, expr)
//            }
//            ExprKind::Literal(l) => {
//                write!(f, "{}", l)
//            }
//            ExprKind::Address(expr) => {
//                write!(f, "&{}", expr)
//            }
//            ExprKind::Deref(expr) => {
//                write!(f, "*{}", expr)
//            }
//            ExprKind::Cast(from, to) => {
//                write!(f, "({} as {})", from, to)
//            }
//            ExprKind::Array(exprs) => {
//                let mut array = String::new();
//                array.push_str("[");
//                for expr in exprs {
//                    array.push_str(&format!("{}, ", expr));
//                }
//                array.push_str("]");
//                write!(f, "{}", array)
//            }
//            ExprKind::StructInit(struct_name, fields) => {
//                let mut struct_init = String::new();
//                struct_init.push_str(&format!("{} {{ ", struct_name));
//                for field in fields {
//                    struct_init.push_str(&format!("{}, ", field));
//                }
//                struct_init.push_str("}");
//                write!(f, "{}", struct_init)
//            }
//            ExprKind::FieldAccess(instance, field_name) => {
//                write!(f, "{}.{}", instance, field_name)
//            }
//            ExprKind::ArrayIndex(instance, index) => {
//                write!(f, "{}[{}]", instance, index)
//            }
//            ExprKind::Call(func, params) => {
//                let mut call = String::new();
//                call.push_str(&format!("{}(", func));
//                for param in params {
//                    call.push_str(&format!("{}, ", param));
//                }
//                call.push_str(")");
//                write!(f, "{}", call)
//            }
//            ExprKind::MethodCall(_, _, _) => todo!(),
//            ExprKind::If(_, _) => todo!(),
//            ExprKind::Loop(_, _) => write!(f, "loop"),
//            ExprKind::Assign(lhs, rhs) => {
//                write!(f, "{} = {}", lhs, rhs)
//            }
//            ExprKind::Var(name) => {
//                write!(f, "{}", name)
//            }
//            ExprKind::Break => todo!(),
//            ExprKind::Return(val) => {
//                if let Some(val) = val {
//                    write!(f, "return {}", val)
//                } else {
//                    write!(f, "return")
//                }
//            }
//        }
//    }
//}
