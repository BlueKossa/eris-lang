
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

impl <'a> Into<Literal<'a>> for LiteralKind<'a> {
    fn into(self) -> Literal<'a> {
        Literal { kind: self }
    }
}