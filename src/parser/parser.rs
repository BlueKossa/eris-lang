#![allow(unused_imports)]
use std::{iter::Peekable, slice::Iter};

use crate::lexer::lexer::LexResult;
use crate::lexer::token::Token;
use crate::lexer::{
    literal::{
        Literal::{self, *},
        Number::{self, *},
    },
    symbol::Symbol::{self, *},
};

use super::ast::{
    blocks::Block,
    expressions::{Expr, ExprKind},
    items::{Item, ItemKind},
    literals::{Literal as AstLiteral, LiteralKind},
    operators::{BinaryOp, UnaryOp},
    statements::Statement,
};

pub struct Parser<'a, I: Iterator> {
    pub tokens: Peekable<I>,
    pub errors: Vec<&'a str>,
}

impl<'a, I: Iterator<Item = LexResult<'a>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        let mut tokens = tokens.peekable();
        Self {
            tokens,
            errors: Vec::new(),
        }
    }

    fn peek_token(&mut self) -> Option<Token<'a>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Some(*token),
            _ => None,
        }
    }

    fn eat_token(&mut self) -> Option<Token<'a>> {
        match self.tokens.next() {
            Some(Ok(token)) => Some(token),
            _ => None,
        }
    }

    pub fn parse_expression(&mut self, precedence: u8) -> Option<Expr<'a>> {
        let left_token = self.peek_token()?;
        let mut lhs: Expr<'a> = match left_token {
            Token::Literal(literal) => {
                self.eat_token();
                match literal {
                    Number(n) => match n {
                        Integer(i) => {
                            ExprKind::Literal(LiteralKind::Int(i.parse().unwrap()).into()).into()
                        }
                        Float(f) => {
                            ExprKind::Literal(LiteralKind::Float(f.parse().unwrap()).into()).into()
                        }
                    },
                    String(s) => ExprKind::Literal(LiteralKind::String(s).into()).into(),
                    Boolean(b) => ExprKind::Literal(LiteralKind::Bool(b).into()).into(),
                }
            }

            Token::Identifier(id) => {
                self.eat_token();
                ExprKind::Var(id).into()
            }

            Token::Symbol(ParenOpen) => {
                self.eat_token();
                let expr = self.parse_expression(0)?;
                if let Token::Symbol(ParenClose) = self.peek_token()? {
                    self.eat_token();
                    expr
                } else {
                    return None;
                }
            }

            _ => return None,
        };

        loop {
            let right_token = match self.peek_token() {
                Some(token) => token,
                None => break,
            };

            if let (Token::Symbol(ParenOpen), Token::Identifier(fn_name)) =
                (right_token, left_token)
            {}

            let (op, p): (BinaryOp, u8) = match right_token.try_into() {
                Ok(op) => {
                    println!("{:?}", op);
                    (op, op.precedence())
                }
                Err(_) => {
                    println!("Err {:?}", right_token);
                    break;
                }
            };

            if p < precedence {
                break;
            }

            self.eat_token()?;

            let rhs = self.parse_expression(p)?;

            if let Some(op) = op.assignment() {
                let expr = ExprKind::Binary(op, lhs.clone(), rhs);
                lhs = ExprKind::Assign(lhs, expr.into()).into();
            } else {
                lhs = ExprKind::Binary(op, lhs, rhs).into();
            }
        }

        Some(lhs)
    }

    fn parse_block(&mut self) -> Option<Block<'a>> {
        let mut statements: Vec<Statement<'a>> = Vec::new();
        loop {
            match self.peek_token
        }

        todo!()
    }

    fn parse_fn_decl(&mut self) -> Option<Item<'a>> {
        todo!()
    }
}
