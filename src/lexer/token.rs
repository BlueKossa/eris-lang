use super::symbol::Symbol;
use super::literal::Literal;


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(Literal<'a>),
    Symbol(Symbol),
    Comment,
    EOF,
}
