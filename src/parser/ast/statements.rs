use super::expressions::Expr;
use super::items::Item;
use super::literals::Literal;


#[derive(Debug, Clone)]
pub enum Statement<'a> {
    Local,
    Item,
    Expression(Expr<'a>),
}
