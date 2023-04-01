use std::{iter::Peekable, str::Chars};

use super::literal::{Literal, Number};
use super::symbol::Symbol;
use super::token::Token;

pub type LexResult<'a> = Result<Token<'a>, String>;

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

    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> &'a str {
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

        &self.source[start..end]
    }

    fn eat_whitespace(&mut self) {
        self.eat_while(|c| c.is_whitespace() || c == '\t' || c == '\r');

    }

    fn eat_identifier(&mut self) -> LexResult<'a> {
        let identifier = self.eat_while(|c| c.is_alphanumeric() || c == '_');

        Ok(Token::Identifier(identifier))
    }

    fn eat_number(&mut self) -> LexResult<'a> {
        let mut dot = false;
        let number = self.eat_while(|c| {
            if c == '.' {
                if dot {
                    return false;
                }
                dot = true;
            }

            c.is_numeric() || c == '.'
        });

        if dot {
            Ok(Token::Literal(Literal::Number(Number::Float(number))))
        } else {
            Ok(Token::Literal(Literal::Number(Number::Integer(number))))
        }
    }

    fn eat_string(&mut self) -> LexResult<'a> {
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

        Ok(Token::Literal(Literal::String(string)))
    }

    fn eat_symbol(&mut self) -> LexResult<'a> {
        let mut done = false;
        let mut len = 0;

        let symbol = self.eat_while(|c| {
            if done || len > 2 {
                return false;
            }

            len += 1;

            return match c {
                '(' | ')' | '{' | '}' | '[' | ']' | ';' | '\n' | ',' | '.' | '@' | '#' => {
                    done = true;
                    true
                }
                ':' | '+' | '-' | '*' | '/' | '%' | '&' | '|' | '~' | '!' | '=' | '<' | '>'
                    if len == 1 =>
                {
                    true
                }
                _ => false,
            };
        });

        Ok(Token::Symbol(symbol.parse().unwrap()))
    }

    fn eat_comment(&mut self) -> LexResult<'a> {
        self.eat_while(|c| c != '\n');
        
        Ok(Token::Comment)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult<'a>;

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
