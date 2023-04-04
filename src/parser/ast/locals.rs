use super::types::Type;

pub struct Local<'a> {
    pub ident: &'a str,
    pub is_mut: bool,
    pub ty: Option<Type<'a>>,
}
