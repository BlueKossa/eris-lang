use super::types::{Type, TypeKind};

#[derive(Debug, Clone)]
pub enum LiteralKind<'a> {
    Int(i64),
    Float(f64),
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
        match self.kind {
            LiteralKind::Int(_) => TypeKind::I32,
            LiteralKind::Float(_) => TypeKind::F32,
            LiteralKind::String(_) => TypeKind::Str,
            LiteralKind::Bool(_) => TypeKind::Bool,
        }.into()
    }
}
