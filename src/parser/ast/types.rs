use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub kind: Box<TypeKind>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
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
    Array(Type, usize),
    Struct(String),
    Ref(Type),
}


impl Type {
    pub fn from_str(name: &str) -> Self {
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
            _ => TypeKind::Struct(name.to_owned()),
        }
        .into()
    }

    pub fn int(signed: bool, size: usize) -> Self {
        match (signed, size) {
            (true, 8) => TypeKind::I8,
            (true, 16) => TypeKind::I16,
            (true, 32) => TypeKind::I32,
            (true, 64) => TypeKind::I64,
            (true, 128) => TypeKind::I128,
            (false, 8) => TypeKind::U8,
            (false, 16) => TypeKind::U16,
            (false, 32) => TypeKind::U32,
            (false, 64) => TypeKind::U64,
            (false, 128) => TypeKind::U128,
            _ => panic!("invalid integer type"),
        }
        .into()
    }

    pub fn float(size: usize) -> Self {
        match size {
            32 => TypeKind::F32,
            64 => TypeKind::F64,
            128 => TypeKind::F128,
            _ => panic!("invalid float type"),
        }
        .into()
    }

    pub fn bool() -> Self {
        TypeKind::Bool.into()
    }

    pub fn char() -> Self {
        TypeKind::Char.into()
    }

    pub fn str() -> Self {
        TypeKind::Str.into()
    }

    pub fn ref_type(ty: Type) -> Self {
        TypeKind::Ref(ty).into()
    }

    pub fn array_type(ty: Type, size: usize) -> Self {
        TypeKind::Array(ty, size).into()
    }

    pub fn inner(&self) -> &TypeKind {
        &*self.kind
    }
}

impl Into<Type> for TypeKind {
    fn into(self) -> Type {
        Type {
            kind: Box::new(self),
        }
    }
}

impl std::fmt::Display for Type {
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
