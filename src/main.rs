mod lexer;
mod parser;
mod codegen;

use inkwell::context::Context;
use lexer::lexer::LexResult;
use lexer::token::Token;
use parser::ast::blocks::Block;
use parser::ast::statements::Statement;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::codegen::visitor::Visitor;


use codegen::codegen::test;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let test_file = std::fs::read_to_string(path).unwrap();
    let lexer = Lexer::new(&test_file);
    let parser = Parser::new(lexer.into_iter());
    let mut block = Block {
        statements: Vec::new(),
    };
    for stmt in parser.into_iter() {
        block.statements.push(stmt.unwrap());
    }

    let ctx = Context::create();
    let mut visitor = Visitor::new(&ctx, "main");
    visitor.traverse(&mut block);
    //visitor.module.print_to_stderr();
    println!("LLVM:");
    visitor.generate_object_file("tst.o");
}
