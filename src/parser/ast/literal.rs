pub struct Literal {
    kind: LiteralKind,
}

pub enum LiteralKind<'a> {
    String(&'a str),

    I64(i64),
    I32(i32),
    I8(i8),
    U64(u64),
    U32(u32),
    U8(u8),
    F64(f64),
    F32(f32),

    Bool(bool),

}
