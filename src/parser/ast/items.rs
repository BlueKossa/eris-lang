use super::blocks::Block;
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
    Foreign(Block<'a>),
}

impl<'a> Into<Item<'a>> for ItemKind<'a> {
    fn into(self) -> Item<'a> {
        Item { kind: self }
    }
}

impl<'a> std::fmt::Display for Item<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ItemKind::Function(function) => write!(f, "{}", function),
            ItemKind::Struct(structure) => write!(f, "{}", structure),
            ItemKind::Constant(ident, ty, expr) => {
                write!(f, "const {}: {} = {}", ident, ty, expr)
            }
            ItemKind::Foreign(block) => write!(f, "foreign {}", block),
        }
    }
}
