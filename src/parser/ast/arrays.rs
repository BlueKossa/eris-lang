use super::types::Type;


#[derive(Debug, Clone, PartialEq)]
pub struct Array<'a> {
    pub ty: Box<Type<'a>>,
    pub len: usize,
}

