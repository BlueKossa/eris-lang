mod lexer;
mod parser;

use lexer::lexer::LexResult;
use lexer::token::Token;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;

fn main() {
    let test_file = std::fs::read_to_string("test.eris").unwrap();
    let mut lexer = Lexer::new("decl main() : i8 {mut x : i8 = 1; x += 1; return 0;}");
    let mut parser = Parser::new(lexer.into_iter());
    let x = parser.parse_identifier().unwrap();
    println!("{:?}", x);
}
