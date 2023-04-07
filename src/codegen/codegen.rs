use std::iter::Peekable;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use crate::parser::ast::items::Item;


pub struct CodeGen<'a, I: Iterator> {
    pub(super) context: &'a Context,
    pub(super) module: Module<'a>,
    pub(super) builder: Builder<'a>,
    pub(super) ast: Peekable<I>,
}


impl<'a, I: Iterator<Item = Item<'a>>> CodeGen<'a, I> {
    fn new(ast: I, module_name: &str, ctx: &'a Context) -> Self {
        let module = ctx.create_module(module_name).clone();
        let builder = ctx.create_builder();
        CodeGen {
            context: ctx,
            module,
            builder,
            ast: ast.peekable(),
        }
    }

    pub(crate) fn peek_item(&mut self) -> Option<&Item<'a>> {
        self.ast.peek()
    }

    pub(crate) fn eat_item(&mut self) -> Option<Item<'a>> {
        self.ast.next()
    }
}


pub fn test() {
    let context = Context::create();
    let mut codegen = CodeGen::new(vec![].into_iter(), "test", &context);
    codegen.create_function();
}
