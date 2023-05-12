use super::blocks::Block;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct FnSig<'a> {
    pub ret: Type<'a>,
    pub args: Vec<(&'a str, Type<'a>)>,
}

#[derive(Debug, Clone)]
pub struct FnDecl<'a> {
    pub name: &'a str,
    pub sig: FnSig<'a>,
    pub body: Block<'a>,
}
