use super::blocks::Block;
use super::literals::Literal;
use super::operators::{BinaryOp, UnaryOp};
use super::types::Type;

#[derive(Debug, Clone)]
pub enum ExprKind<'a> {
    Binary(BinaryOp, Expr<'a>, Expr<'a>),
    Unary(UnaryOp, Expr<'a>),
    Literal(Literal<'a>),
    Address(Expr<'a>),
    Deref(Expr<'a>),
    Cast(Expr<'a>, Type<'a>),
    Array(Vec<Expr<'a>>),
    StructInit(&'a str, Vec<Expr<'a>>),
    FieldAccess(Expr<'a>, &'a str),
    ArrayIndex(Expr<'a>, Expr<'a>),
    Call(&'a str, Vec<Expr<'a>>),
    MethodCall(Expr<'a>, &'a str, Vec<Expr<'a>>),
    If(Expr<'a>, Block<'a>),
    Loop(Expr<'a>, Block<'a>),
    Assign(Expr<'a>, Expr<'a>),
    Var(&'a str),
    Break,
    Return(Option<Expr<'a>>),
}

impl<'a> From<ExprKind<'a>> for Expr<'a> {
    fn from(val: ExprKind<'a>) -> Self {
        Expr {
            kind: Box::new(val),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr<'a> {
    pub kind: Box<ExprKind<'a>>,
}

impl<'a> std::fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                write!(f, "({} {} {})", lhs, op, rhs)
            }
            ExprKind::Unary(op, expr) => {
                write!(f, "({}{})", op, expr)
            }
            ExprKind::Literal(l) => {
                write!(f, "{}", l)
            }
            ExprKind::Address(expr) => {
                write!(f, "&{}", expr)
            }
            ExprKind::Deref(expr) => {
                write!(f, "*{}", expr)
            }
            ExprKind::Cast(from, to) => {
                write!(f, "({} as {})", from, to)
            }
            ExprKind::Array(exprs) => {
                let mut array = String::new();
                array.push_str("[");
                for expr in exprs {
                    array.push_str(&format!("{}, ", expr));
                }
                array.push_str("]");
                write!(f, "{}", array)
            }
            ExprKind::StructInit(struct_name, fields) => {
                let mut struct_init = String::new();
                struct_init.push_str(&format!("{} {{ ", struct_name));
                for field in fields {
                    struct_init.push_str(&format!("{}, ", field));
                }
                struct_init.push_str("}");
                write!(f, "{}", struct_init)
            }
            ExprKind::FieldAccess(instance, field_name) => {
                write!(f, "{}.{}", instance, field_name)
            }
            ExprKind::ArrayIndex(instance, index) => {
                write!(f, "{}[{}]", instance, index)
            }
            ExprKind::Call(func, params) => {
                let mut call = String::new();
                call.push_str(&format!("{}(", func));
                for param in params {
                    call.push_str(&format!("{}, ", param));
                }
                call.push_str(")");
                write!(f, "{}", call)
            }
            ExprKind::MethodCall(_, _, _) => todo!(),
            ExprKind::If(_, _) => todo!(),
            ExprKind::Loop(_, _) => todo!(),
            ExprKind::Assign(lhs, rhs) => {
                write!(f, "{} = {}", lhs, rhs)
            }
            ExprKind::Var(name) => {
                write!(f, "{}", name)
            }
            ExprKind::Break => todo!(),
            ExprKind::Return(val) => {
                if let Some(val) = val {
                    write!(f, "return {}", val)
                } else {
                    write!(f, "return")
                }
            }
        }
    }
}
