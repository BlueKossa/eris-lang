use super::symbol::Symbol;
use super::literal::Literal;


#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(Literal<'a>),
    Symbol(Symbol),
    Whitespace,
    EOF,
}
