use super::expressions::Expr;

pub enum Statement<'a> {
    Local,
    Item,
    Expression(Expr<'a>),
}
