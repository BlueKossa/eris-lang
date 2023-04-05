use super::types::Type;
use super::expressions::Expr;

#[derive(Debug, Clone)]
pub struct Local<'a> {
    pub ident: &'a str,
    pub is_mut: bool,
    pub ty: Option<Type<'a>>,
    pub value: Option<Expr<'a>>,
}
