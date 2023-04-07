use super::codegen::CodeGen;
use crate::parser::ast::{
    items::{Item, ItemKind},
    functions::FnDecl,
};



impl<'a, I: Iterator<Item = Item<'a>>> CodeGen<'a, I> {
    pub fn create_function(&mut self) {
            let i64_type = self.context.i64_type();
            let fn_type = i64_type.fn_type(&[i64_type.into()], false);
            let function = self.module.add_function("test", fn_type, None);
            let basic_block = self.context.append_basic_block(function, "entry");

            self.builder.position_at_end(basic_block);

            self.builder.build_return(Some(&i64_type.const_int(0, false)));

            self.module.print_to_stderr();
    }
}
