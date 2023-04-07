mod lexer;
mod parser;
mod codegen;

use lexer::lexer::LexResult;
use lexer::token::Token;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;


use codegen::codegen::test;

fn main() {
    let test_file = std::fs::read_to_string("test.eris").unwrap();
    let mut lexer = Lexer::new("decl main(s : i8) : i8 {
                                    mut x : i8 = 1;
                                    x += 1; 
                                    return 0;
                                }
                                decl test(s : i8, t : i8) {
                                    return s * t;
                                    }");
    let mut parser = Parser::new(lexer.into_iter());
    let x = parser.into_iter();
    // Print length of iterator
    for i in x {
        println!("Item:\n");
        println!("{:?}", i);
    }
    test();
}
