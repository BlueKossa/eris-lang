use super::expressions::Expr;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct Local<'a> {
    pub ident: &'a str,
    pub is_mut: bool,
    pub ty: Option<Type<'a>>,
    pub value: Option<Expr<'a>>,
}

impl<'a> std::fmt::Display for Local<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut local = String::new();
        if self.is_mut {
            local.push_str("mut ");
        }
        local.push_str(&format!("{}: ", self.ident));
        if let Some(ty) = &self.ty {
            local.push_str(&format!("{}", ty));
        }
        if let Some(value) = &self.value {
            local.push_str(&format!(" = {}", value));
        }
        write!(f, "{}", local)
    }
}
