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
    pub tokens: Peekable<I>,
    pub errors: Vec<&'a str>,
    pub last_token: Option<Token<'a>>,
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
        let tokens = tokens.peekable();
        Self {
            tokens,
            errors: Vec::new(),
            last_token: None,
        }
    }

    fn peek_token(&mut self) -> ParseResult<'a, Token<'a>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(*token),
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    fn eat_token(&mut self) -> ParseResult<'a, Token<'a>> {
        match self.tokens.next() {
            Some(Ok(token)) => {
                self.last_token = Some(token);
                Ok(token)
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::LexError("Unexpected end of file"),
            }),
        }
    }

    pub fn parse_expression(&mut self, precedence: u8) -> ParseResult<'a, Expr<'a>> {
        let left_token = self.peek_token()?;

        let mut lhs: Expr<'a> = match left_token {
            Token::Literal(literal) => {
                self.eat_token()?;
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
                self.eat_token()?;
                match id {
                    "true" => ExprKind::Literal(LiteralKind::Bool(true).into()).into(),
                    "false" => ExprKind::Literal(LiteralKind::Bool(false).into()).into(),
                    _ => match self.peek_token()? {
                        Token::Symbol(BraceOpen) if precedence == 0 => {
                            self.eat_token()?;
                            let mut fields = Vec::new();
                            loop {
                                match self.peek_token()? {
                                    Token::Symbol(BraceClose) => {
                                        self.eat_token()?;
                                        break;
                                    }
                                    Token::Symbol(Comma) => {
                                        self.eat_token()?;
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
                self.eat_token()?;
                let expr = self.parse_expression(0)?;
                if let Token::Symbol(ParenClose) = self.peek_token()? {
                    self.eat_token()?;
                    expr
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                    });
                }
            }

            Token::Symbol(BracketOpen) => {
                self.eat_token()?;
                let mut array = Vec::new();
                loop {
                    match self.peek_token()? {
                        Token::Symbol(BracketClose) => {
                            self.eat_token()?;
                            break;
                        }
                        Token::Symbol(Comma) => {
                            self.eat_token()?;
                        }
                        _ => {
                            let expr = self.parse_expression(0)?;
                            array.push(expr);
                        }
                    }
                }
                ExprKind::Array(array).into()
            }
            _ => {
                println!("Unexpected token: {:?}", self.last_token);
                return Err(ParseError {
                    kind: ParseErrorKind::NotAnExpression(left_token),
                });
            }
        };

        loop {
            let right_token = match self.peek_token() {
                Ok(Token::Symbol(ColonEqual))
                | Ok(Token::Symbol(Colon))
                | Ok(Token::Symbol(ColonColon)) => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                    })
                }
                Ok(Token::Symbol(Semicolon)) => break,
                Ok(token) => token,
                Err(_) => break,
            };
            if let Token::Symbol(Dot) = right_token {
                self.eat_token()?;
                let field = match self.peek_token()? {
                    Token::Identifier(id) => id,
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                        })
                    }
                };
                self.eat_token()?;
                lhs = ExprKind::FieldAccess(lhs, field).into();
                continue;
            } else if let Token::Symbol(BracketOpen) = right_token {
                self.eat_token()?;
                let index = self.parse_expression(0)?;
                if let Token::Symbol(BracketClose) = self.peek_token()? {
                    self.eat_token()?;
                    lhs = ExprKind::ArrayAccess(lhs, index).into();
                    continue;
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                    });
                }
            }

            if let (Token::Symbol(ParenOpen), Token::Identifier(_)) = (right_token, left_token) {
                return self.parse_fn_call();
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

            self.eat_token()?;

            let rhs = self.parse_expression(p)?;

            if let Some(op) = op.assignment() {
                let expr = ExprKind::Binary(op, lhs.clone(), rhs);

                lhs = ExprKind::Assign(lhs, expr.into()).into();
            } else if let BinaryOp::Assign = op {
                println!("ASSIGN");
                println!("lhs: {:?}", lhs);
                println!("rhs: {:?}", rhs);
                lhs = ExprKind::Assign(lhs, rhs).into();
            } else {
                lhs = ExprKind::Binary(op, lhs, rhs).into();
            }
        }

        Ok(lhs)
    }

    fn parse_type(&mut self) -> ParseResult<'a, Type<'a>> {
        let token = self.peek_token()?;
        match token {
            Token::Identifier(name) => {
                self.eat_token()?;
                Ok(Type::from_str(name))
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            }),
        }
    }

    fn parse_local_decl(&mut self, name: &'a str, is_mut: bool) -> ParseResult<'a, Local<'a>> {
        let expr;
        let mut ty = None;
        match self.peek_token()? {
            Token::Symbol(Colon) => {
                self.eat_token()?;
                ty = Some(self.parse_type()?);
                if let Token::Symbol(Equal) = self.peek_token()? {
                    self.eat_token()?;
                } else {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                    });
                }
            }
            Token::Symbol(ColonEqual) => {
                self.eat_token()?;
            }
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                })
            }
        }

        expr = Some(self.parse_expression(0)?);

        Ok(Local {
            ident: name,
            ty,
            is_mut,
            value: expr,
        })
    }

    fn parse_mut_local_decl(&mut self) -> ParseResult<'a, Local<'a>> {
        self.eat_token()?;
        let name = match self.peek_token()? {
            Token::Identifier(name) => name,
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                })
            }
        };
        self.eat_token()?;
        return self.parse_local_decl(name, true);
    }

    fn parse_return(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.eat_token()?;
        if let Token::Symbol(Semicolon) = self.peek_token()? {
            return Ok(ExprKind::Return(None).into());
        }
        let expr = self.parse_expression(0)?;
        Ok(ExprKind::Return(Some(expr)).into())
    }

    fn parse_while(&mut self) -> ParseResult<'a, Expr<'a>> {
        self.eat_token()?;
        println!("Expr");
        let cond = self.parse_expression(0)?;
        println!("Body");
        let body = self.parse_block()?;
        Ok(ExprKind::Loop(cond, body).into())
    }

    pub fn parse_identifier(&mut self) -> ParseResult<'a, Statement<'a>> {
        if let Token::Identifier(ident) = self.peek_token()? {
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
                        self.eat_token()?;
                        let b = AstLiteral {
                            kind: LiteralKind::Bool(true),
                        };
                        let cond = ExprKind::Literal(b).into();
                        let loop_expr = self.parse_block()?;
                        return Ok(Statement::Expression(ExprKind::Loop(cond, loop_expr).into()));
                    }
                    Break => {
                        self.eat_token()?;
                        return Ok(Statement::Expression(ExprKind::Break.into()));
                    }
                    If => {
                        self.eat_token()?;
                        let cond = self.parse_expression(0)?;
                        let body = self.parse_block()?;
                        return Ok(Statement::Expression(ExprKind::If(
                            cond,
                            body,
                        )
                        .into()));

                    }
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                        })
                    }
                }
            } else {
                let expr = self.parse_expression(0);
                match expr {
                    Ok(expr) => {
                        return Ok(Statement::Expression(expr));
                    }
                    Err(e) => match self.peek_token()? {
                        Token::Symbol(ColonEqual) | Token::Symbol(Colon) => {
                            let local = self.parse_local_decl(ident, false)?;
                            return Ok(Statement::Local(local));
                        }
                        Token::Symbol(ColonColon) => {
                            self.eat_token()?;
                            match self.peek_token()? {
                                Token::Identifier(name) => match Keyword::from_str(name) {
                                    Some(Struct) => {
                                        let struct_decl = self.parse_struct_decl(ident)?;
                                        return Ok(Statement::Item(
                                            ItemKind::Struct(struct_decl).into(),
                                        ));
                                    }
                                    _ => {
                                        todo!("Constant & Union declaration")
                                    }
                                },
                                Token::Symbol(ParenOpen) => {
                                    let func = self.parse_fn_decl(ident)?;
                                    return Ok(Statement::Item(ItemKind::Function(func).into()));
                                }
                                _ => {
                                    return Err(ParseError {
                                        kind: ParseErrorKind::UnexpectedToken(
                                            self.last_token.unwrap(),
                                        ),
                                    })
                                }
                            }
                        }
                        _ => return Err(e),
                    },
                }
            }
        }
        Err(ParseError {
            kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
        })
    }

    fn parse_fn_call(&mut self) -> ParseResult<'a, Expr<'a>> {
        let fn_name = if let Token::Identifier(name) = self.last_token.unwrap() {
            name
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        };
        self.eat_token()?;
        let mut args: Vec<Expr<'a>> = Vec::new();
        loop {
            match self.peek_token()? {
                Token::Symbol(ParenClose) => {
                    self.eat_token()?;
                    break;
                }
                Token::Symbol(Semicolon) => {
                    self.eat_token()?;
                }
                Token::Symbol(Comma) => {
                    self.eat_token()?;
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
        self.eat_token()?;
        let mut statements: Vec<Statement<'a>> = Vec::new();
        loop {
            match self.peek_token()? {
                Token::Symbol(BraceClose) => {
                    self.eat_token()?;
                    break;
                }
                Token::Symbol(Semicolon) => {
                    self.eat_token()?;
                }
                // Attempt to parse an expression, if there is a :=, parse a local decl
                Token::Identifier(_) => {
                    let statement = self.parse_identifier()?;
                    statements.push(statement);
                }
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                    })
                }
            }
        }
        Ok(Block { statements })
    }

    fn parse_arg(&mut self) -> ParseResult<'a, (&'a str, Type<'a>)> {
        let name;
        if let Token::Identifier(n) = self.peek_token()? {
            self.eat_token()?;
            name = n;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        }
        if let Token::Symbol(Colon) = self.peek_token()? {
            self.eat_token()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        }
        let mut ty = None;
        loop {
            match self.peek_token()? {
                Token::Identifier(t) => {
                    self.eat_token()?;
                    ty = Some(Type::from_str(t));
                }
                Token::Symbol(Comma) | Token::Symbol(ParenClose) | Token::Symbol(BraceClose) => {
                    break;
                }
                t => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t),
                    });
                }
            }
        }
        return Ok((name, ty.unwrap()));
    }

    fn parse_fn_header(&mut self) -> ParseResult<'a, FnSig<'a>> {
        self.eat_token()?;
        let mut args = Vec::new();
        loop {
            match self.peek_token()? {
                Token::Identifier(_) => {
                    let arg = self.parse_arg()?;
                    args.push(arg);
                }
                Token::Symbol(Comma) => {
                    self.eat_token()?;
                }
                Token::Symbol(ParenClose) => {
                    self.eat_token()?;
                    break;
                }
                t => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t),
                    })
                }
            }
        }
        let mut ret = TypeKind::Void.into();
        if let Token::Symbol(Colon) = self.peek_token()? {
            self.eat_token()?;
            if let Token::Identifier(t) = self.peek_token()? {
                self.eat_token()?;
                ret = Type::from_str(t);
            } else {
                return Err(ParseError {
                    kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
                });
            }
        }

        return Ok(FnSig { args, ret });
    }

    fn parse_fn_decl(&mut self, name: &'a str) -> ParseResult<'a, FnDecl<'a>> {
        let sig;

        if let Token::Symbol(ParenOpen) = self.peek_token()? {
            sig = self.parse_fn_header()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        }

        let block;
        if let Token::Symbol(BraceOpen) = self.peek_token()? {
            block = self.parse_block()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        }
        return Ok(FnDecl {
            sig,
            name,
            body: block,
        });
    }

    fn parse_struct_decl(&mut self, name: &'a str) -> ParseResult<'a, Struct<'a>> {
        self.eat_token()?;
        if let Token::Symbol(BraceOpen) = self.peek_token()? {
            self.eat_token()?;
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::UnexpectedToken(self.last_token.unwrap()),
            });
        }
        let mut fields: Vec<Field> = Vec::new();
        loop {
            dbg!(self.peek_token()?);
            match self.peek_token()? {
                Token::Identifier(_) => {
                    let field = self.parse_arg()?;
                    dbg!("PUSHING FIELD");
                    fields.push(Field {
                        name: field.0,
                        ty: field.1,
                    });
                }
                Token::Symbol(Comma) => {
                    self.eat_token()?;
                }
                Token::Symbol(BraceClose) => {
                    self.eat_token()?;
                    break;
                }
                t => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken(t),
                    })
                }
            }
        }
        return Ok(Struct { name, fields });
    }
}

impl<'a, I: Iterator<Item = LexResult<'a>>> Iterator for Parser<'a, I> {
    type Item = ParseResult<'a, Statement<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peek_token() {
            Ok(Token::EOF) => None,
            Ok(_) => Some(self.parse_identifier()),
            Err(_) => None,
        }
    }
}
