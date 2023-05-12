use super::literal::Literal;
use super::symbol::Symbol;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Identifier(&'a str),
    Literal(Literal<'a>),
    Symbol(Symbol),
    Comment,
    EOF,
}
