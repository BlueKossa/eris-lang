use super::types::Type;


#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Field<'a>>,
}
