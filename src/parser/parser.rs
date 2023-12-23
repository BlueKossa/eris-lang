#![allow(unused_imports)]
use std::{iter::Peekable, slice::Iter};

use itertools::{peek_nth, PeekNth};

use crate::lexer::lexer::LexResult;
use crate::lexer::token::{Token, TokenKind};
use crate::lexer::{
    literal::{
        Literal as LLit,
        Number as LNum,
    },
    symbol::Symbol::{self, *},
};
use crate::span::Span;

use super::ast::functions::{FnDecl, FnSig};
use super::ast::locals::Local;
use super::ast::structs::Field;
use super::ast::types::{Type, TypeKind};
use super::ast::{
    blocks::Block,
    expressions::{Expr, ExprKind},
    items::{Item, ItemKind},
    keywords::Keyword as KW,
    literals::{Literal, LiteralKind},
    operators::{BinaryOp, UnaryOp},
    statements::Statement,
    structs::Struct,
};

pub struct Parser<'a, I: Iterator> {
    pub tokens: PeekNth<I>,
    source: &'a str,
    pub errors: Vec<&'a str>,
}

#[derive(Debug)]
enum ParseErrorKind<'a> {
    UnexpectedToken(Token),
    LexError(&'a str),
    NotAnExpression(Token),
}

#[derive(Debug)]
pub struct ParseError<'a> {
    kind: ParseErrorKind<'a>,
}

type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl<'a, I: Iterator<Item = LexResult>> Parser<'a, I> {
    pub fn new(tokens: I, source: &'a str) -> Self {
        let tokens = peek_nth(tokens);
        Self {
            tokens,
            source,
            errors: Vec::new(),
        }
    }

    fn peek(&mut self) -> ParseResult<'a, Token> {
        self.peek_nth(0)
    }

    fn peek_nth(&mut self, n: usize) -> ParseResult<'a, Token> {
        match self.tokens.peek_nth(n) {
            Some(Ok(token)) => Ok(*token),
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    fn eat(&mut self) -> ParseResult<'a, Token> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(token),
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    pub fn parse_expression(&mut self, precedence: u8) -> ParseResult<'a, Expr> {
        let left_token = self.peek()?;

        let mut lhs: Expr = match left_token.kind {
            TokenKind::Literal(literal) => {
                self.eat()?;
                match literal {
                    LLit::Number(n) => match n {
                        LNum::Integer{signed, size} => {
                            let literal = Literal {
                                kind: LiteralKind::Int,
                                span: left_token.span,
                                type_: Type::int(signed, size),
                            };
                            ExprKind::Literal(literal).into()
                        }
                        LNum::Float{size} => {
                            let literal = Literal {
                                kind: LiteralKind::Float,
                                span: left_token.span,
                                type_: Type::float(size),
                            };
                            ExprKind::Literal(literal).into()
                        }
                    },
                    LLit::String => {
                        let literal = Literal {
                            kind: LiteralKind::String,
                            span: left_token.span,
                            type_: Type::str(),
                        };
                        ExprKind::Literal(literal).into()
                    }
                    LLit::Boolean => {
                        let literal = Literal {
                            kind: LiteralKind::Bool,
                            span: left_token.span,
                            type_: Type::bool(),
                        };
                        ExprKind::Literal(literal).into()
                    }
                }
            }

            TokenKind::Identifier => {
                let id = left_token.span.data(self.source);
                self.eat()?;
                match id {
                    "true" | "false" => {
                        let literal = Literal {
                            kind: LiteralKind::Bool,
                            span: left_token.span,
                            type_: Type::bool(),
                        };
                        ExprKind::Literal(literal).into()
                    }
                    _ => match self.peek()?.kind {
                        TokenKind::Symbol(BraceOpen) if precedence == 0 => {
                            self.eat()?;
                            let mut fields = Vec::new();
                            loop {
                                match self.peek()?.kind {
                                    TokenKind::Symbol(BraceClose) => {
                                        self.eat()?;
                                        break;
                                    }
                                    TokenKind::Symbol(Comma) => {
                                        self.eat()?;
                                    }
                                    _ => {
                                        let field = self.parse_expression(0)?;
                                        fields.push(field);
                                    }
                                }
                            }
                            ExprKind::StructInit(left_token.span, fields).into()
                        }
                        _ => ExprKind::Var(left_token.span).into(),
                    },
                }
            }
            TokenKind::Symbol(ParenOpen) => {
                self.eat()?;
                let expr = self.parse_expression(0)?;
                if let TokenKind::Symbol(ParenClose) = self.peek()?.kind {
                    self.eat()?;
                    expr
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    });
                }
            }
            TokenKind::Symbol(And) => {
                self.eat()?;
                let expr = self.parse_expression(0)?;
                ExprKind::Address(expr).into()
            }
            TokenKind::Symbol(BracketOpen) => {
                self.eat()?;
                let mut array = Vec::new();
                loop {
                    match self.peek()?.kind {
                        TokenKind::Symbol(BracketClose) => {
                            self.eat()?;
                            break;
                        }
                        TokenKind::Symbol(Comma) => {
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
            TokenKind::Symbol(Asterisk) => {
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
            let right_token = if let Ok(token) = self.peek() {
                token
            } else {
                break;
            };
            match right_token.kind {
                TokenKind::Symbol(ColonEqual)
                | TokenKind::Symbol(Colon)
                | TokenKind::Symbol(ColonColon) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                    })
                }
                TokenKind::Symbol(Semicolon) => break,
                _ => (),
            }

            match right_token.kind {
                TokenKind::Symbol(Dot) => {
                    self.eat()?;
                    let field = match self.peek()?.kind {
                        TokenKind::Identifier => {
                            self.peek()?.span
                        }
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
                TokenKind::Symbol(BracketOpen) => {
                    self.eat()?;
                    let index = self.parse_expression(0)?;
                    if let TokenKind::Symbol(BracketClose) = self.peek()?.kind {
                        self.eat()?;
                        lhs = ExprKind::ArrayIndex(lhs, index).into();
                        continue;
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                        });
                    }
                }
                TokenKind::Symbol(DotAsterisk) => {
                    self.eat()?;
                    lhs = ExprKind::Deref(lhs).into();
                    continue;
                }
                TokenKind::Identifier => {
                    let ident = self.peek()?.span.data(self.source);
                    if ident == "as" {
                        self.eat()?;
                        let ty = self.parse_type()?;
                        lhs = ExprKind::Cast(lhs, ty).into();
                        continue;
                    }
                }
                _ => (),
            }

            if let (TokenKind::Symbol(ParenOpen), TokenKind::Identifier) =
                (right_token.kind, left_token.kind)
            {
                let fn_name = left_token.span;
                return self.parse_fn_call(fn_name);
            }

            let (op, p): (BinaryOp, u8) = match right_token.kind.try_into() {
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

    fn parse_type(&mut self) -> ParseResult<'a, Type> {
        let token = self.peek()?;

        let mut ty = match token.kind {
            TokenKind::Identifier => {
                let name = token.span.data(self.source);
                self.eat()?;
                Ok(Type::from_str(name))
            }
            TokenKind::Symbol(And) => {
                self.eat()?;
                let ty = self.parse_type()?;
                Ok(Type::ref_type(ty))
            }
            t => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(token),
            }),
        };

        loop {
            match self.peek()?.kind {
                TokenKind::Symbol(Semicolon)
                | TokenKind::Symbol(Equal)
                | TokenKind::Symbol(ParenClose)
                | TokenKind::Symbol(ParenOpen)
                | TokenKind::Symbol(BracketClose)
                | TokenKind::Symbol(Comma) => break,
                TokenKind::Symbol(And) => {
                    self.eat()?;
                    ty = Ok(Type::ref_type(ty?));
                }
                TokenKind::Symbol(BracketOpen) => {
                    self.eat()?;
                    let size_token = self.eat()?;

                    let size = if let TokenKind::Literal(lit) = size_token.kind {
                        if let LLit::Number(_) = lit {
                            let data = size_token.span.data(self.source);
                            Some(data.parse::<usize>().unwrap())
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    if let Some(size) = size {
                        self.eat()?;
                        ty = Ok(Type::array_type(ty?, size));
                    } else {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                        });
                    }
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

    fn parse_local_decl(&mut self, is_mut: bool) -> ParseResult<'a, Local> {
        let token = self.peek()?;
        let name = match token.kind {
            TokenKind::Identifier => token.span,
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                })
            }
        };
        self.eat()?;
        let mut ty = None;
        match self.peek()?.kind {
            TokenKind::Symbol(Colon) => {
                self.eat()?;
                ty = Some(self.parse_type()?);
                if let TokenKind::Symbol(Equal) = self.peek()?.kind {
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
            TokenKind::Symbol(ColonEqual) => {
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

    fn parse_mut_local_decl(&mut self) -> ParseResult<'a, Local> {
        self.eat()?;
        return self.parse_local_decl(true);
    }

    fn parse_return(&mut self) -> ParseResult<'a, Expr> {
        self.eat()?;
        if let TokenKind::Symbol(Semicolon) = self.peek()?.kind {
            return Ok(ExprKind::Return(None).into());
        }
        let expr = self.parse_expression(0)?;
        Ok(ExprKind::Return(Some(expr)).into())
    }

    fn parse_while(&mut self) -> ParseResult<'a, Expr> {
        self.eat()?;
        let cond = self.parse_expression(0)?;
        let body = self.parse_block()?;
        Ok(ExprKind::Loop(cond, body).into())
    }

    pub fn parse_identifier(&mut self) -> ParseResult<'a, Statement> {
        let token = self.peek()?;
        if let TokenKind::Identifier = token.kind {
            let ident = token.span;

            if let Some(keyword) = KW::from_str(ident.data(self.source)) {
                match keyword {
                    KW::Mut => {
                        let local = self.parse_mut_local_decl()?;
                        return Ok(Statement::Local(local));
                    }
                    KW::Return => {
                        let expr = self.parse_return()?;
                        return Ok(Statement::Expression(expr));
                    }
                    //Decl => {
                    //    let func = self.parse_fn_decl()?;
                    //    return Ok(Statement::Item(ItemKind::Function(func).into()));
                    //}
                    KW::While => {
                        let while_loop = self.parse_while()?;
                        return Ok(Statement::Expression(while_loop));
                    }
                    KW::Loop => {
                        self.eat()?;
                        let b = Literal {
                            kind: LiteralKind::Bool,
                            span: Span::empty(),
                            type_: Type::bool(),
                        };
                        let cond = ExprKind::Literal(b).into();
                        let loop_expr = self.parse_block()?;
                        return Ok(Statement::Expression(
                            ExprKind::Loop(cond, loop_expr).into(),
                        ));
                    }
                    KW::Break => {
                        self.eat()?;
                        return Ok(Statement::Expression(ExprKind::Break.into()));
                    }
                    KW::If => {
                        self.eat()?;
                        let cond = self.parse_expression(0)?;
                        let body = self.parse_block()?;
                        return Ok(Statement::Expression(ExprKind::If(cond, body).into()));
                    }
                    KW::Foreign => {
                        self.eat()?;
                        let block = self.parse_block()?;
                        return Ok(Statement::Item(ItemKind::Foreign(block).into()));
                    }
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.peek()?),
                        })
                    }
                }
            } else {
                match self.peek_nth(1)?.kind {
                    TokenKind::Symbol(ColonEqual) | TokenKind::Symbol(Colon) => {
                        let local = self.parse_local_decl(false)?;
                        return Ok(Statement::Local(local));
                    }
                    TokenKind::Symbol(ColonColon) => {
                        // Eat ident and :: such as:
                        // {main ::} ()...
                        self.eat()?;
                        self.eat()?;
                        let token = self.peek()?;
                        match token.kind {
                            TokenKind::Identifier => {
                                let name = token.span.data(self.source);
                                match KW::from_str(name) {
                                    Some(KW::Struct) => {
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
                                if let TokenKind::Symbol(ParenOpen) = self.peek()?.kind {
                                    let func = self.parse_fn_decl(ident, ty)?;
                                    return Ok(Statement::Item(ItemKind::Function(func).into()));
                                }
                            }
                            TokenKind::Symbol(And) => {
                                let ty = self.parse_type()?;
                                if let TokenKind::Symbol(ParenOpen) = self.peek()?.kind {
                                    let func = self.parse_fn_decl(ident, ty)?;
                                    return Ok(Statement::Item(ItemKind::Function(func).into()));
                                }
                            }
                            TokenKind::Symbol(ParenOpen) => {
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

    fn parse_fn_call(&mut self, fn_name: Span) -> ParseResult<'a, Expr> {
        self.eat()?;
        let mut args: Vec<Expr> = Vec::new();
        loop {
            match self.peek()?.kind {
                TokenKind::Symbol(ParenClose) => {
                    self.eat()?;
                    break;
                }
                TokenKind::Symbol(Semicolon) => {
                    self.eat()?;
                }
                TokenKind::Symbol(Comma) => {
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

    fn parse_block(&mut self) -> ParseResult<'a, Block> {
        self.eat()?;
        let mut statements: Vec<Statement> = Vec::new();
        loop {
            match self.peek()?.kind {
                TokenKind::Symbol(BraceClose) => {
                    self.eat()?;
                    break;
                }
                TokenKind::Symbol(Semicolon) => {
                    self.eat()?;
                }
                // Attempt to parse an expression, if there is a :=, parse a local decl
                TokenKind::Identifier => {
                    let statement = self.parse_identifier()?;
                    statements.push(statement);
                }
                TokenKind::Comment => {
                    self.eat()?;
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

    fn parse_arg(&mut self) -> ParseResult<'a, (Span, Type)> {
        let name;
        let token = self.peek()?;
        if let TokenKind::Identifier = self.peek()?.kind {
            self.eat()?;
            name = token.span;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        if let TokenKind::Symbol(Colon) = self.peek()?.kind {
            self.eat()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }
        let ty = self.parse_type()?;
        Ok((name, ty))
    }

    fn parse_fn_header(&mut self, ret: Type) -> ParseResult<'a, FnSig> {
        self.eat()?;
        let mut args = Vec::new();
        loop {
            let token = self.peek()?;
            match token.kind {
                TokenKind::Identifier => {
                    let arg = self.parse_arg()?;
                    args.push(arg);
                }
                TokenKind::Symbol(Comma) => {
                    self.eat()?;
                }
                TokenKind::Symbol(ParenClose) => {
                    self.eat()?;
                    break;
                }
                TokenKind::Symbol(Ellipsis) => {
                    self.eat()?;
                    self.eat()?;
                    return Ok(FnSig {
                        args,
                        ret,
                        is_variadic: true,
                    });
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(token),
                    })
                }
            }
        }

        Ok(FnSig {
            args,
            ret,
            is_variadic: false,
        })
    }

    fn parse_fn_decl(&mut self, name: Span, fn_type: Type) -> ParseResult<'a, FnDecl> {
        let sig;

        if let TokenKind::Symbol(ParenOpen) = self.peek()?.kind {
            sig = self.parse_fn_header(fn_type)?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.peek()?),
            });
        }

        let block;
        let token = self.peek()?;
        match token.kind {
            TokenKind::Symbol(BraceOpen) => {
                block = self.parse_block()?;
            }
            TokenKind::Symbol(Semicolon) => {
                self.eat()?;
                block = Block {
                    statements: Vec::new(),
                };
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(token),
                })
            }
        }
        Ok(FnDecl {
            sig,
            name,
            body: block,
        })
    }

    fn parse_struct_decl(&mut self, name: Span) -> ParseResult<'a, Struct> {
        self.eat()?;
        let token = self.peek()?;
        if let TokenKind::Symbol(BraceOpen) = token.kind {
            self.eat()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(token),
            });
        }
        let mut fields: Vec<Field> = Vec::new();
        loop {
            let token = self.peek()?;
            match token.kind {
                TokenKind::Identifier => {
                    let field = self.parse_arg()?;
                    fields.push(Field {
                        name: field.0,
                        ty: field.1,
                    });
                }
                TokenKind::Symbol(Semicolon) => {
                    self.eat()?;
                }
                TokenKind::Symbol(BraceClose) => {
                    self.eat()?;
                    break;
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(token),
                    })
                }
            }
        }
        Ok(Struct { name, fields })
    }
}

impl<'a, I: Iterator<Item = LexResult>> Iterator for Parser<'a, I> {
    type Item = ParseResult<'a, Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek().map(|t| t.kind) {
            Ok(TokenKind::EOF) => None,
            Ok(_) => Some(self.parse_identifier()),
            Err(_) => None,
        }
    }
}
