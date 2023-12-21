use super::types::{Type, TypeKind};

#[derive(Debug, Clone)]
pub enum LiteralKind<'a> {
    Int(i64, Type<'a>),
    Float(f64, Type<'a>),
    String(&'a str),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub struct Literal<'a> {
    pub kind: LiteralKind<'a>,
}

impl<'a> Into<Literal<'a>> for LiteralKind<'a> {
    fn into(self) -> Literal<'a> {
        Literal { kind: self }
    }
}

impl<'a> Literal<'a> {
    pub fn to_ty(&self) -> Type<'a> {
        match &self.kind {
            LiteralKind::Int(_, ty) => ty.inner().clone(),
            LiteralKind::Float(_, ty) => ty.inner().clone(),
            LiteralKind::String(_) => TypeKind::Str,
            LiteralKind::Bool(_) => TypeKind::Bool,
        }
        .into()
    }
}

impl<'a> std::fmt::Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            LiteralKind::Int(int, ty) => write!(f, "{}_{}", int, ty),
            LiteralKind::Float(float, ty) => write!(f, "{}_{}", float, ty),
            LiteralKind::String(string) => write!(f, "{}", string),
            LiteralKind::Bool(boolean) => write!(f, "{}", boolean),
        }
    }
}
