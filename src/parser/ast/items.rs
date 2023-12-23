use crate::span::Span;

use super::blocks::Block;
use super::expressions::Expr;
use super::functions::FnDecl;
use super::structs::Struct;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(FnDecl),
    Struct(Struct),
    Constant(Span, Type, Expr),
    Foreign(Block),
}

impl Into<Item> for ItemKind {
    fn into(self) -> Item {
        Item { kind: self }
    }
}

//impl std::fmt::Display for Item {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match &self.kind {
//            ItemKind::Function(function) => write!(f, "{}", function),
//            ItemKind::Struct(structure) => write!(f, "{}", structure),
//            ItemKind::Constant(ident, ty, expr) => {
//                write!(f, "const {}: {} = {}", ident, ty, expr)
//            }
//            ItemKind::Foreign(block) => write!(f, "foreign {}", block),
//        }
//    }
//}
