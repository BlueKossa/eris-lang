use std::{iter::Peekable, str::Chars};

use crate::span::Span;

use super::literal::{Literal, Number};

use super::symbol::Symbol;
use super::token::{Token, TokenKind};

pub type LexResult = Result<Token, String>;

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            chars: source.chars().peekable(),
            line: 1,
            column: 1,
            index: 0,
        }
    }

    fn eat(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.column += 1;
            self.index += 1;

            if c == '\n' {
                self.line += 1;
                self.column = 1;
            }

            return Some(c);
        }

        None
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().cloned()
    }

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> Span {
        let start = self.index;
        let mut end = start;

        loop {
            if let Some(c) = self.peek() {
                if !predicate(c) {
                    break;
                }

                end += 1;

                let _ = self.eat();
            } else {
                break;
            }
        }

        Span::new(start, end)
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(|c| c.is_whitespace() || c == '\t' || c == '\r');
    }

    fn eat_identifier(&mut self) -> LexResult {
        let identifier = self.eat_while(|c| c.is_alphanumeric() || c == '_');
        let token = Token {
            span: identifier,
            kind: TokenKind::Identifier,
        };

        Ok(token)
    }

    fn eat_number(&mut self) -> LexResult {
        let mut dot = false;
        let mut type_specifier = false;
        let mut number = self.eat_while(|c| {
            match c {
                '.' => {
                    if dot {
                        return false;
                    }
                    dot = true;
                }
                '_' => {
                    println!("type_specifier = true");
                    type_specifier = true;
                    return true;
                }
                _ => {}
            }
            println!("c = {}", c);
            if type_specifier && c.is_alphabetic() {
                println!("type_specifier = false");
                return true;
            }
            c.is_numeric() || c == '.'
        });

        let number_kind = if dot {
            Number::Float {
                size: 64,
            }
        } else {
            Number::Integer {
                signed: true,
                size: 64,
            }
        };
        let token = Token {
            span: number,
            kind: TokenKind::Literal(Literal::Number(number_kind)),
        };
        Ok(token)
    }

    fn eat_string(&mut self) -> LexResult {
        let mut quotes = 0;
        let mut escaped = false;
        let string = self.eat_while(|c| {
            if quotes == 2 {
                return false;
            }

            match c {
                '\"' | '\'' if !escaped => {
                    quotes += 1;
                    escaped = false;
                    true
                }
                '\\' if !escaped => {
                    escaped = true;
                    true
                }
                _ => {
                    escaped = false;
                    true
                }
            }
        });

        if quotes != 2 {
            return Err(format!("Expected 2 quotes, got {}", quotes));
        }
        let token = Token {
            span: string,
            kind: TokenKind::Literal(Literal::String),
        };
        Ok(token)
    }

    fn eat_symbol(&mut self) -> LexResult {
        let mut done = false;
        let mut len = 0;

        let symbol_span = self.eat_while(|c| {
            if done || len > 2 {
                return false;
            }

            len += 1;

            match c {
                '(' | ')' | '{' | '}' | '[' | ']' | ';' | '\n' | ',' | '@' | '#' => {
                    done = true;
                    len == 1
                }
                ':' | '.' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '~' | '!' | '=' | '<'
                | '>' => true,
                _ => false,
            }
        });

        let symbol: Symbol = symbol_span.data(self.source).parse().unwrap();
        let token = Token {
            span: symbol_span,
            kind: TokenKind::Symbol(symbol),
        };
        Ok(token)
    }

    fn eat_comment(&mut self) -> LexResult {
        let span = self.eat_while(|c| c != '\n');
        let token = Token {
            span,
            kind: TokenKind::Comment,
        };
        Ok(token)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();
        let c = match self.peek() {
            Some(c) => c,
            None => {
                return None;
            }
        };

        let token = match c {
            'a'..='z' | 'A'..='Z' | '_' => self.eat_identifier(),
            '0'..='9' => self.eat_number(),
            '\"' | '\'' => self.eat_string(),
            '$' => self.eat_comment(),
            '(' | ')' | '{' | '}' | '[' | ']' | ';' | '\n' | ',' | '.' | '@' | '#' | ':' | '+'
            | '-' | '*' | '/' | '%' | '&' | '|' | '~' | '!' | '=' | '<' | '>' => self.eat_symbol(),
            _ => {
                return Some(Err(format!("Unexpected character: {}", c)));
            }
        };

        Some(token)
    }
}
