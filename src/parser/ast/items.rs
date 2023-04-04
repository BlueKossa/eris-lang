use super::functions::FnDecl;
use super::types::Type;
use super::structs::Struct;
use super::expressions::Expr;


#[derive(Debug, Clone)]
pub struct Item<'a> {
    kind: ItemKind<'a>,
}


#[derive(Debug, Clone)]
pub enum ItemKind<'a> {
    Function(FnDecl<'a>),
    Struct(Struct<'a>),
    Constant(&'a str, Type<'a>, Expr<'a>),
}

