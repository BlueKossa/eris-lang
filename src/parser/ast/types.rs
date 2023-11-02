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


impl<'a> std::fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self.kind {
            TypeKind::Void => write!(f, "void"),
            TypeKind::I8 => write!(f, "i8"),
            TypeKind::I16 => write!(f, "i16"),
            TypeKind::I32 => write!(f, "i32"),
            TypeKind::I64 => write!(f, "i64"),
            TypeKind::I128 => write!(f, "i128"),
            TypeKind::U8 => write!(f, "u8"),
            TypeKind::U16 => write!(f, "u16"),
            TypeKind::U32 => write!(f, "u32"),
            TypeKind::U64 => write!(f, "u64"),
            TypeKind::U128 => write!(f, "u128"),
            TypeKind::F32 => write!(f, "f32"),
            TypeKind::F64 => write!(f, "f64"),
            TypeKind::F128 => write!(f, "f128"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Str => write!(f, "str"),
            TypeKind::Array(t, s) => write!(f, "[{}; {}]", t, s),
            TypeKind::Struct(name) => write!(f, "struct({})", name),
            TypeKind::Ref(t) => write!(f, "&{}", t),
        }
    }
}
