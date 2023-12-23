use crate::span::Span;

use super::{literal::Literal, symbol::Symbol};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Identifier,
    Literal(Literal),
    Symbol(Symbol),
    Comment,
    EOF,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }

    pub fn get_identifier(&self) -> Option<Span> {
        match self.kind {
            TokenKind::Identifier => Some(self.span),
            _ => None,
        }
    }
}
