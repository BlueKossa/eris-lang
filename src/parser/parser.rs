#![allow(unused_imports)]
use std::{iter::Peekable, slice::Iter};

use itertools::{peek_nth, PeekNth};

use crate::lexer::lexer::LexResult;
use crate::lexer::token::Token;
use crate::lexer::{
    literal::{
        Literal::{self, *},
        Number::{self, *},
    },
    symbol::Symbol::{self, *},
};

use super::ast::functions::{FnDecl, FnSig};
use super::ast::locals::Local;
use super::ast::structs::Field;
use super::ast::types::{Type, TypeKind};
use super::ast::{
    blocks::Block,
    expressions::{Expr, ExprKind},
    items::{Item, ItemKind},
    keywords::Keyword::{self, *},
    literals::{Literal as AstLiteral, LiteralKind},
    operators::{BinaryOp, UnaryOp},
    statements::Statement,
    structs::Struct,
};

pub struct Parser<'a, I: Iterator> {
    pub tokens: PeekNth<I>,
    pub errors: Vec<&'a str>,
}

#[derive(Debug)]
enum ParseErrorKind<'a> {
    UnexpectedToken(Token<'a>),
    LexError(&'a str),
    NotAnExpression(Token<'a>),
}

#[derive(Debug)]
pub struct ParseError<'a> {
    kind: ParseErrorKind<'a>,
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl<'a, I: Iterator<Item = LexResult<'a>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        let tokens = peek_nth(tokens);
        Self {
            tokens,
            errors: Vec::new(),
        }
    }

    fn peek(&mut self) -> ParseResult<'a, Token<'a>> {
        self.peek_nth(0)
    }

    fn peek_nth(&mut self, n: usize) -> ParseResult<'a, Token<'a>> {
        match self.tokens.peek_nth(n) {
            Some(Ok(token)) => Ok(*token),
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    fn eat(&mut self) -> ParseResult<'a, Token<'a>> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(token),
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    pub fn parse_expression(&mut self, precedence: u8) -> ParseResult<'a, Expr<'a>> {
        let left_token = self.peek()?;

        let mut lhs: Expr<'a> = match left_token {
            Token::Literal(literal) => {
                self.eat()?;
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
                self.eat()?;
                match id {
                    "true" => ExprKind::Literal(LiteralKind::Bool(true).into()).into(),
                    "false" => ExprKind::Literal(LiteralKind::Bool(false).into()).into(),
                    _ => match self.peek()? {
                        Token::Symbol(BraceOpen) if precedence == 0 => {
                            self.eat()?;
                            let mut fields = Vec::new();
                            loop {
                                match self.peek()? {
                                    Token::Symbol(BraceClose) => {
                                        self.eat()?;
                                        break;
                                    }
                                    Token::Symbol(Comma) => {
                                        self.eat()?;
                                    }
                                    _ => {
                                        let field = self.parse_expression(0)?;
                                        fields.push(field);
                                    }
                                }
                            }
                            ExprKind::StructInit(id, fields).into()
                        }
                        _ => ExprKind::Var(id).into(),
                    },
                }
            }
            Token::Symbol(ParenOpen) => {
                self.eat()?;
                let expr = self.parse_expression(0)?;
                if let Token::Symbol(ParenClose) = self.peek()? {
                    self.eat()?;
                    expr
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    });
                }
            }
            Token::Symbol(And) => {
                self.eat()?;
                let expr = self.parse_expression(0)?;
                ExprKind::Address(expr).into()
            }
            Token::Symbol(BracketOpen) => {
                self.eat()?;
                let mut array = Vec::new();
                loop {
                    match self.peek()? {
                        Token::Symbol(BracketClose) => {
                            self.eat()?;
                            break;
                        }
                        Token::Symbol(Comma) => {
                            self.eat()?;
                        }
                        _ => {
                            let expr = self.parse_expression(0)?;
                            array.push(expr);
                        }
                    }
                }
                ExprKind::Array(array).into()
            }
            Token::Symbol(Asterisk) => {
                self.eat()?;
                let expr = self.parse_expression(0)?;
                ExprKind::Deref(expr).into()
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::NotAnExpression(left_token),
                });
            }
        };

        loop {
            let right_token = match self.peek() {
                Ok(Token::Symbol(ColonEqual))
                | Ok(Token::Symbol(Colon))
                | Ok(Token::Symbol(ColonColon)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    })
                }
                Ok(Token::Symbol(Semicolon)) => break,
                Ok(token) => token,
                Err(_) => break,
            };

            match right_token {
                Token::Symbol(Dot) => {
                    self.eat()?;
                    let field = match self.peek()? {
                        Token::Identifier(id) => id,
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                            })
                        }
                    };
                    self.eat()?;
                    lhs = ExprKind::FieldAccess(lhs, field).into();
                    continue;
                }
                Token::Symbol(BracketOpen) => {
                    self.eat()?;
                    let index = self.parse_expression(0)?;
                    if let Token::Symbol(BracketClose) = self.peek()? {
                        self.eat()?;
                        lhs = ExprKind::ArrayIndex(lhs, index).into();
                        continue;
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                        });
                    }
                }
                Token::Symbol(DotAsterisk) => {
                    self.eat()?;
                    lhs = ExprKind::Deref(lhs).into();
                    continue;
                }
                Token::Identifier(ident) => {
                    if ident == "as" {
                        self.eat()?;
                        let ty = self.parse_type()?;
                        lhs = ExprKind::Cast(lhs, ty).into();
                        continue;
                    }
                }
                _ => (),
            }

            if let (Token::Symbol(ParenOpen), Token::Identifier(fn_name)) =
                (right_token, left_token)
            {
                return self.parse_fn_call(fn_name);
            }

            let (op, p): (BinaryOp, u8) = match right_token.try_into() {
                Ok(op) => (op, op.precedence()),
                Err(_) => {
                    break;
                }
            };

            if p < precedence {
                break;
            }

            self.eat()?;

            let rhs = self.parse_expression(p)?;

            if let Some(op) = op.assignment() {
                let expr = ExprKind::Binary(op, lhs.clone(), rhs);

                lhs = ExprKind::Assign(lhs, expr.into()).into();
            } else if let BinaryOp::Assign = op {
                lhs = ExprKind::Assign(lhs, rhs).into();
            } else {
                lhs = ExprKind::Binary(op, lhs, rhs).into();
            }
        }

        Ok(lhs)
    }

    fn parse_type(&mut self) -> ParseResult<'a, Type<'a>> {
        let token = self.peek()?;

        let mut ty = match token {
            Token::Identifier(name) => {
                self.eat()?;
                Ok(Type::from_str(name))
            }
            Token::Symbol(And) => {
                self.eat()?;
                let ty = self.parse_type()?;
                Ok(Type::ref_type(ty))
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            }),
        };

        loop {
            match self.peek()? {
                Token::Symbol(Semicolon)
                | Token::Symbol(Equal)
                | Token::Symbol(ParenClose)
                | Token::Symbol(ParenOpen)
                | Token::Symbol(BracketClose)
                | Token::Symbol(Comma) => break,
                Token::Symbol(And) => {
                    self.eat()?;
                    ty = Ok(Type::ref_type(ty?));
                }
                Token::Symbol(BracketOpen) => {
                    self.eat()?;
                    let size = self.eat()?;

                    let size = match size {
                        Token::Literal(Number(Integer(n))) => n.parse::<usize>().unwrap(),
                        _ => {
                            return Err(ParseError {
                                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                            })
                        }
                    };
                    self.eat()?;
                    ty = Ok(Type::array_type(ty?, size));
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    })
                }
            }
        }
        ty
    }

    fn parse_local_decl(&mut self, is_mut: bool) -> ParseResult<'a, Local<'a>> {
        let name = match self.peek()? {
            Token::Identifier(name) => name,
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                })
            }
        };
        self.eat()?;
        let mut ty = None;
        match self.peek()? {
            Token::Symbol(Colon) => {
                self.eat()?;
                ty = Some(self.parse_type()?);
                if let Token::Symbol(Equal) = self.peek()? {
                    self.eat()?;
                } else {
                    return Ok(Local {
                        ident: name,
                        ty,
                        is_mut,
                        value: None,
                    });
                }
            }
            Token::Symbol(ColonEqual) => {
                self.eat()?;
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                })
            }
        }

        let expr = Some(self.parse_expression(0)?);

        Ok(Local {
            ident: name,
            ty,
            is_mut,
            value: expr,
        })
    }

    fn parse_mut_local_decl(&mut self) -> ParseResult<'a, Local<'a>> {
        self.eat()?;
        return self.parse_local_decl(true);
    }

    fn parse_return(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.eat()?;
        if let Token::Symbol(Semicolon) = self.peek()? {
            return Ok(ExprKind::Return(None).into());
        }
        let expr = self.parse_expression(0)?;
        Ok(ExprKind::Return(Some(expr)).into())
    }

    fn parse_while(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.eat()?;
        let cond = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(ExprKind::Loop(cond, body).into())
    }

    pub fn parse_identifier(&mut self) -> ParseResult<'a, Statement<'a>> {
        if let Token::Identifier(ident) = self.peek()? {
            if let Some(keyword) = Keyword::from_str(ident) {
                match keyword {
                    Mut => {
                        let local = self.parse_mut_local_decl()?;
                        return Ok(Statement::Local(local));
                    }
                    Return => {
                        let expr = self.parse_return()?;
                        return Ok(Statement::Expression(expr));
                    }
                    //Decl => {
                    //    let func = self.parse_fn_decl()?;
                    //    return Ok(Statement::Item(ItemKind::Function(func).into()));
                    //}
                    While => {
                        let while_loop = self.parse_while()?;
                        return Ok(Statement::Expression(while_loop));
                    }
                    Loop => {
                        self.eat()?;
                        let b = AstLiteral {
                            kind: LiteralKind::Bool(true),
                        };
                        let cond = ExprKind::Literal(b).into();
                        let loop_expr = self.parse_block()?;
                        return Ok(Statement::Expression(
                            ExprKind::Loop(cond, loop_expr).into(),
                        ));
                    }
                    Break => {
                        self.eat()?;
                        return Ok(Statement::Expression(ExprKind::Break.into()));
                    }
                    If => {
                        self.eat()?;
                        let cond = self.parse_expression(0)?;
                        let body = self.parse_block()?;
                        return Ok(Statement::Expression(ExprKind::If(cond, body).into()));
                    }
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                        })
                    }
                }
            } else {
                match self.peek_nth(1)? {
                    Token::Symbol(ColonEqual) | Token::Symbol(Colon) => {
                        let local = self.parse_local_decl(false)?;
                        return Ok(Statement::Local(local));
                    }
                    Token::Symbol(ColonColon) => {
                        // Eat ident and :: such as:
                        // {main ::} ()...
                        self.eat()?;
                        self.eat()?;
                        match self.peek()? {
                            Token::Identifier(name) => {
                                match Keyword::from_str(name) {
                                    Some(Struct) => {
                                        let struct_decl = self.parse_struct_decl(ident)?;
                                        return Ok(Statement::Item(
                                            ItemKind::Struct(struct_decl).into(),
                                        ));
                                    }
                                    Some(_) => {
                                        return Err(ParseError {
                                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                                        })
                                    }
                                    _ => {}
                                }
                                let ty = self.parse_type()?;
                                dbg!(&ty);
                                dbg!(self.peek()?);
                                if let Token::Symbol(ParenOpen) = self.peek()? {
                                    let func = self.parse_fn_decl(ident, ty)?;
                                    return Ok(Statement::Item(ItemKind::Function(func).into()));
                                }
                            }
                            Token::Symbol(ParenOpen) => {
                                let func = self.parse_fn_decl(ident, TypeKind::Void.into())?;
                                return Ok(Statement::Item(ItemKind::Function(func).into()));
                            }
                            _ => {
                                return Err(ParseError {
                                    kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                                })
                            }
                        }
                    }
                    _ => {
                        let expr = self.parse_expression(0)?;
                        return Ok(Statement::Expression(expr));
                    }
                }
            }
        }
        Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
        })
    }

    fn parse_fn_call(&mut self, fn_name: &'a str) -> ParseResult<'a, Expr<'a>> {
        self.eat()?;
        let mut args: Vec<Expr<'a>> = Vec::new();
        loop {
            match self.peek()? {
                Token::Symbol(ParenClose) => {
                    self.eat()?;
                    break;
                }
                Token::Symbol(Semicolon) => {
                    self.eat()?;
                }
                Token::Symbol(Comma) => {
                    self.eat()?;
                }
                _ => {
                    let expr = self.parse_expression(0)?;
                    args.push(expr);
                }
            }
        }
        Ok(ExprKind::Call(fn_name, args).into())
    }

    fn parse_block(&mut self) -> ParseResult<'a, Block<'a>> {
        self.eat()?;
        let mut statements: Vec<Statement<'a>> = Vec::new();
        loop {
            match self.peek()? {
                Token::Symbol(BraceClose) => {
                    self.eat()?;
                    break;
                }
                Token::Symbol(Semicolon) => {
                    self.eat()?;
                }
                // Attempt to parse an expression, if there is a :=, parse a local decl
                Token::Identifier(_) => {
                    let statement = self.parse_identifier()?;
                    statements.push(statement);
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    })
                }
            }
        }
        Ok(Block { statements })
    }

    fn parse_arg(&mut self) -> ParseResult<'a, (&'a str, Type<'a>)> {
        let name;
        if let Token::Identifier(n) = self.peek()? {
            self.eat()?;
            name = n;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        if let Token::Symbol(Colon) = self.peek()? {
            self.eat()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        let ty = self.parse_type()?;
        Ok((name, ty))
    }

    fn parse_fn_header(&mut self, ret: Type<'a>) -> ParseResult<'a, FnSig<'a>> {
        self.eat()?;
        let mut args = Vec::new();
        loop {
            match self.peek()? {
                Token::Identifier(_) => {
                    let arg = self.parse_arg()?;
                    args.push(arg);
                }
                Token::Symbol(Comma) => {
                    self.eat()?;
                }
                Token::Symbol(ParenClose) => {
                    self.eat()?;
                    break;
                }
                t => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t),
                    })
                }
            }
        }

        Ok(FnSig { args, ret })
    }

    fn parse_fn_decl(&mut self, name: &'a str, fn_type: Type<'a>) -> ParseResult<'a, FnDecl<'a>> {
        let sig;

        if let Token::Symbol(ParenOpen) = self.peek()? {
            sig = self.parse_fn_header(fn_type)?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }

        let block;
        if let Token::Symbol(BraceOpen) = self.peek()? {
            block = self.parse_block()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        Ok(FnDecl {
            sig,
            name,
            body: block,
        })
    }

    fn parse_struct_decl(&mut self, name: &'a str) -> ParseResult<'a, Struct<'a>> {
        self.eat()?;
        if let Token::Symbol(BraceOpen) = self.peek()? {
            self.eat()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        let mut fields: Vec<Field> = Vec::new();
        loop {
            match self.peek()? {
                Token::Identifier(_) => {
                    let field = self.parse_arg()?;
                    fields.push(Field {
                        name: field.0,
                        ty: field.1,
                    });
                }
                Token::Symbol(Comma) => {
                    self.eat()?;
                }
                Token::Symbol(BraceClose) => {
                    self.eat()?;
                    break;
                }
                t => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t),
                    })
                }
            }
        }
        Ok(Struct { name, fields })
    }
}

impl<'a, I: Iterator<Item = LexResult<'a>>> Iterator for Parser<'a, I> {
    type Item = ParseResult<'a, Statement<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek() {
            Ok(Token::EOF) => None,
            Ok(_) => Some(self.parse_identifier()),
            Err(_) => None,
        }
    }
}
