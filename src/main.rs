mod parser;
mod lexer;

use lexer::token::Token;

use crate::lexer::lexer::Lexer;


fn main() {
    let test_file = std::fs::read_to_string("test.eris").unwrap();
    let mut lexer = Lexer::new(&test_file);
    let tokens = lexer.into_iter().collect::<Result<Vec<Token>, _>>().unwrap();

    for token in tokens {
        println!("{:?}", token);
    }
}
