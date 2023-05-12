use super::expressions::Expr;
use super::functions::FnDecl;
use super::structs::Struct;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct Item<'a> {
    pub kind: ItemKind<'a>,
}

#[derive(Debug, Clone)]
pub enum ItemKind<'a> {
    Function(FnDecl<'a>),
    Struct(Struct<'a>),
    Constant(&'a str, Type<'a>, Expr<'a>),
}

impl<'a> Into<Item<'a>> for ItemKind<'a> {
    fn into(self) -> Item<'a> {
        Item { kind: self }
    }
}
