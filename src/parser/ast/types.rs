
#[derive(Debug, Clone)]
pub enum Type<'a> {
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
    Tuple(Vec<Type<'a>>),
    Array(Box<Type<'a>>, usize),
    Slice(Box<Type<'a>>),
    Struct(&'a str),
}
