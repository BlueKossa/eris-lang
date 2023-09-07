use crate::parser::ast::{
    blocks::Block, expressions::ExprKind, functions::FnDecl, items::Item, locals::Local,
    statements::Statement,
};

pub trait MutVisitorPattern<'a> where Self: ExpressionVisitor<'a> {
    type ReturnType;
    fn traverse_block(&mut self, block: &mut Block<'a>) -> Self::ExprReturnType;

    fn traverse_statement(&mut self, statement: &mut Statement<'a>) -> Self::ExprReturnType;

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ExprReturnType;

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ExprReturnType;

    fn traverse_function(&mut self, function: &mut FnDecl<'a>) -> Self::ExprReturnType;
}


pub trait Visitor<'a> where Self: ExpressionVisitor<'a>  {
    type ReturnType;
}



pub trait ExpressionVisitor<'a> {
    type ExprReturnType;
    fn visit_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_binary(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_unary(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_literal(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_address_of(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_deref(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_array(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_struct(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_field(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_index(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_call(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_if(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_loop(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_assign(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_var(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_break(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
    fn visit_return(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType;
}
