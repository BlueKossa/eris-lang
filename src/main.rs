mod lexer;
mod parser;

use lexer::token::Token;

use crate::lexer::lexer::Lexer;
use crate::parser::parser::Parser;

fn main() {
    let test_file = std::fs::read_to_string("test.eris").unwrap();
    let mut lexer = Lexer::new("1 * 2 + 3");
    let mut parser = Parser::new(lexer.into_iter());
    let expr = parser.parse_expression(0).unwrap();
    println!("{:?}", expr);
}
