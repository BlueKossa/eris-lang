use super::types::Type;
use super::blocks::Block;

pub struct FnSig {
    pub ret: Type,
    pub args: Vec<Type>,
}

pub struct FnDecl<'a> {
    pub name: &'a str,
    pub sig: FnSig,
    pub body: Block<'a>,
}
