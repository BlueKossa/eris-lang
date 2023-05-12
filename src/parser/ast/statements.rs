use super::expressions::Expr;
use super::items::Item;
use super::locals::Local;

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Local(Local<'a>),
    Item(Item<'a>),
    Expression(Expr<'a>),
}
