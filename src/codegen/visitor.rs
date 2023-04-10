use std::collections::HashMap;

use inkwell::{module::Module, builder::Builder, context::Context, values::BasicValueEnum};

use crate::parser::ast::{blocks::Block, statements::Statement, items::{Item, ItemKind}, functions::FnDecl};

pub struct Visitor<'a> {
    builder: Builder<'a>,
    context: &'a Context,
    module: &'a Module<'a>,
    values: HashMap<&'a str, BasicValueEnum<'a>>,
}


struct VisitorResult<'a>{
    value: Option<BasicValueEnum<'a>>,
}

impl Default for VisitorResult<'_> {
    fn default() -> Self {
        VisitorResult {
            value: None,
        }
    }
}

impl<'a> VisitorResult<'a> {
    fn new(value: Option<BasicValueEnum<'a>>) -> Self {
        VisitorResult {
            value,
        }
    }
}


impl<'a> Visitor<'a> {
    pub fn new(context: &'a Context, module: &'a Module<'a>) -> Self {
        let builder = context.create_builder();
        Visitor {
            builder,
            context,
            module,
            values: HashMap::new(),
        }
    }

    fn traverse_block(&mut self, block: &mut Block<'a>) -> VisitorResult<'a> {
        for statement in block.statements.iter_mut() {

        }
    }
    
    fn traverse_statement(&mut self, stmt: &mut Statement<'a>) -> VisitorResult<'a> {
        match stmt {
            Statement::Item(item) => {
                self.traverse_item(item)
            },
            Statement::Expression(expr) => {
                self.traverse_expression(expr)
            },
            Statement::Local(local) => {
                self.traverse_local(local)
            },
        };
        VisitorResult::default()
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> VisitorResult<'a> {
        match item.kind {
            ItemKind::Function(func) => {
                self.traverse_function(func)
            },
            ItemKind::Struct(_) => {
                todo!()
            },
            ItemKind::Constant(_) => {
                todo!()
            }
        }
        VisitorResult::default()
    }

    fn traverse_function(&mut self, func: &mut FnDecl<'a>) -> VisitorResult<'a> {

    }

    pub fn traverse(&mut self, block: &mut Block<'a>) -> VisitorResult<'a> {}
}
