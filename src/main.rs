mod codegen;
mod lexer;
mod parser;
mod visitor;
mod semantic;
mod module;
mod span;

use inkwell::context::Context;


use parser::ast::blocks::Block;

//use semantic::semantic_visitor::SemanticVisitor;

use crate::codegen::visitor::CodeGenVisitor;
use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;

fn main() {
    let ctx = Context::create();
    let mut compiler = CodeGenVisitor::new(&ctx);
    compiler.run("test.eris");
}
