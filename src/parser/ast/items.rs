use super::functions::FnDecl;
use super::types::Type;
use super::structs::Struct;
use super::expressions::Expr;

pub struct Item<'a> {
    kind: ItemKind<'a>,
}

pub enum ItemKind<'a> {
    Function(FnDecl<'a>),
    Struct(Struct<'a>),
    Constant(&'a str, Type, Expr<'a>),
}

