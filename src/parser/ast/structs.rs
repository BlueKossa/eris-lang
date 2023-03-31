use super::types::Type;

pub struct Field<'a> {
    pub name: &'a str,
    pub ty: Type,
}

pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Field<'a>>,
}
