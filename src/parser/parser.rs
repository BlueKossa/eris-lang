use std::{iter::Peekable, slice::Iter};

use crate::lexer::token::Token;
use crate::lexer::lexer::LexResult;

pub struct Parser<'a, I: Iterator> {
    pub tokens: Peekable<I>,
    pub errors: Vec<&'a str>,
}

impl <'a, I: Iterator<Item=LexResult<'a>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        let mut tokens = tokens.peekable();
        Self {
            tokens,
            errors: Vec::new(),
        }
    }


}
