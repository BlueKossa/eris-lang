pub enum LiteralKind<'a> {
    Int(i64),
    Float(f64),
    String(&'a str),
    Bool(bool),
}

pub struct Literal<'a> {
    pub kind: LiteralKind<'a>,
}
