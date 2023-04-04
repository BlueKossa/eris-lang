use super::types::Type;
use super::blocks::Block;


#[derive(Debug, Clone)]
pub struct FnSig<'a> {
    pub ret: Type<'a>,
    pub args: Vec<Type<'a>>,
}


#[derive(Debug, Clone)]
pub struct FnDecl<'a> {
    pub name: &'a str,
    pub sig: FnSig<'a>,
    pub body: Block<'a>,
}
