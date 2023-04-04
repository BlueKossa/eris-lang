use super::expressions::Expr;
use super::items::Item;


#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Local(Expr<'a>, Expr<'a>),
    Item(Item<'a>),
    Expression(Expr<'a>),
}
