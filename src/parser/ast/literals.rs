use crate::span::Span;

use super::types::{Type, TypeKind};

#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int,
    Float,
    String,
    Bool,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
    pub type_: Type,
}

impl Literal {
    pub fn to_ty(&self) -> Type {
        self.type_.clone()
    }
}

//impl std::fmt::Display for Literal {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        //match &self.kind {
//        //    LiteralKind::Int(int, ty) => write!(f, "{}_{}", int, ty),
//        //    LiteralKind::Float(float, ty) => write!(f, "{}_{}", float, ty),
//        //    LiteralKind::String(string) => write!(f, "{}", string),
//        //    LiteralKind::Bool(boolean) => write!(f, "{}", boolean),
//        //}
//        write!(f, "{}", self.span)
//    }
//}
