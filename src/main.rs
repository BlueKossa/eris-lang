mod lexer;
mod parser;
mod codegen;
mod visitor;
mod semantic;

use inkwell::context::Context;
use lexer::lexer::LexResult;
use lexer::token::Token;
use parser::ast::blocks::Block;
use parser::ast::statements::Statement;
use semantic::visitor::SemanticVisitor;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::codegen::visitor::CodeGenVisitor;


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

    //let ctx = Context::create();
    //let mut visitor = Visitor::new(&ctx, "main");
    //visitor.traverse(&mut block);
    //visitor.module.print_to_stderr();
    //println!("LLVM:");
    //visitor.generate_object_file("tst.o");
    println!("AST Before: {:?}", block);
    let mut semantic_visitor = SemanticVisitor::new();
    semantic_visitor.run(&mut block);
    let structs = semantic_visitor.structs;
    let fn_decls = semantic_visitor.fn_decls;
    let ctx = Context::create();
    let mut codegen_visitor = CodeGenVisitor::new(&ctx, "main");
    codegen_visitor.declare_structs(&structs);
    codegen_visitor.declare_functions(&fn_decls);
    codegen_visitor.run(&mut block);
    codegen_visitor.dump();
    codegen_visitor.generate_machine_code("tst.o");
}

