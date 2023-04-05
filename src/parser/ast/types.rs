
#[derive(Debug, Clone)]
pub enum Type<'a> {
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
    Struct(&'a str),
}


impl<'a> Type<'a> {
    pub fn from_str(name: &'a str) -> Self {
        match name {
            "void" => Type::Void,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "i128" => Type::I128,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "u128" => Type::U128,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "f128" => Type::F128,
            "bool" => Type::Bool,
            "char" => Type::Char,
            "str" => Type::Str,
            _ => Type::Struct(name),
        }

    }
}
