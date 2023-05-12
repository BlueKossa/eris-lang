use super::expressions::Expr;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct Local<'a> {
    pub ident: &'a str,
    pub is_mut: bool,
    pub ty: Option<Type<'a>>,
    pub value: Option<Expr<'a>>,
}
