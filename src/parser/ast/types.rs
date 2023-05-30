#[derive(Debug, Clone, PartialEq)]
pub struct Type<'a> {
    pub kind: Box<TypeKind<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind<'a> {
    Void,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    F128,
    Bool,
    Char,
    Str,
    Array(Type<'a>, usize),
    Struct(&'a str),
    Ref(Type<'a>),
}

impl<'a> Type<'a> {
    pub fn from_str(name: &'a str) -> Self {
        match name {
            "void" => TypeKind::Void,
            "i8" => TypeKind::I8,
            "i16" => TypeKind::I16,
            "i32" => TypeKind::I32,
            "i64" => TypeKind::I64,
            "i128" => TypeKind::I128,
            "u8" => TypeKind::U8,
            "u16" => TypeKind::U16,
            "u32" => TypeKind::U32,
            "u64" => TypeKind::U64,
            "u128" => TypeKind::U128,
            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,
            "f128" => TypeKind::F128,
            "bool" => TypeKind::Bool,
            "char" => TypeKind::Char,
            "str" => TypeKind::Str,
            _ => TypeKind::Struct(name),
        }
        .into()
    }

    pub fn ref_type(ty: Type<'a>) -> Self {
        TypeKind::Ref(ty).into()
    }

    pub fn array_type(ty: Type<'a>, size: usize) -> Self {
        TypeKind::Array(ty, size).into()
    }
}

impl<'a> Into<Type<'a>> for TypeKind<'a> {
    fn into(self) -> Type<'a> {
        Type {
            kind: Box::new(self),
        }
    }
}
