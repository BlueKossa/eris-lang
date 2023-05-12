use crate::parser::ast::{
    blocks::Block, expressions::ExprKind, functions::FnDecl, items::Item, locals::Local,
    statements::Statement,
};

pub trait MutVisitorPattern<'a> {
    type ReturnType;
    fn traverse_block(&mut self, block: &mut Block<'a>) -> Self::ReturnType;

    fn traverse_statement(&mut self, statement: &mut Statement<'a>) -> Self::ReturnType;

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ReturnType;

    fn traverse_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ReturnType;

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ReturnType;

    fn traverse_function(&mut self, function: &mut FnDecl<'a>) -> Self::ReturnType;
}
