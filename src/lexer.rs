use std::{path::Path, str::CharIndices, fs, iter::Peekable};



type LexerResult = Result<Token, String>;

enum Numeric {
    Int(i32),
    Float(f32)
}

enum Literal {
    Numeric(Numeric),
    Bool(bool),
    Char(char),
    String(String),
}

enum Keyword {
    Fn,

    Sit,
    Const,

    If,
    Else,

    For,
    While,
    Return,
    Break,
    Continue,
    Pass,
}

enum Symbol {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,

    Dot,
    Comma,
    Colon,
    Semicolon,

    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Power,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    And,
    Or,
    Not,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    BitwiseLeftShift,
    BitwiseRightShift,
    BitwiseUnsignedRightShift,

    Increment,
    Decrement,

    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

enum Token {
    EOF,
    Numeric(Numeric),
    Literal(Literal),
    Keyword(Keyword),
    Symbol(Symbol),
    Ident(String),
}

struct Lexer<'a>{
    chars: Peekable<CharIndices<'a>>,
    input: &'a str,
    x_pos: usize,
    y_pos: usize,
}

trait ToToken {
    fn to_token(&self) -> Token;
}


impl ToToken for Numeric {
    fn to_token(&self) -> Token {
        Token::Numeric(*self)
    }
}

impl ToToken for Literal {
    fn to_token(&self) -> Token {
        Token::Literal(*self)
    }
}

impl ToToken for Keyword {
    fn to_token(&self) -> Token {
        Token::Keyword(*self)
    }
}

impl ToToken for Symbol {
    fn to_token(&self) -> Token {
        Token::Symbol(*self)
    }
}


impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            chars: input.char_indices().peekable().clone(),
            x_pos: 0,
            y_pos: 0,
            input: &input,
        };
        l
    }

    fn read_next(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn get_next(&mut self) -> Option<char> {
        if let Some(c) = self.chars.next() {
            self.x_pos += 1;

            if c.1 == '\n' {
                self.x_pos = 0;
                self.y_pos += 1;
            }

            return Some(c.1);
        }

        None
    }

    fn get_while<F>(&mut self, mut f: F) -> String 
    where F: FnMut(char) -> bool {
        let mut s = String::new();

        while let Some(c) = self.read_next() {
            if f(c) {
                s.push(self.get_next().unwrap());
            } else {
                break;
            }
        }

        s
    }

    fn get_ident(&mut self) -> Token {
        let s = self.get_while(|c| c.is_alphabetic() || c.is_numeric() || c == '_');
        Token::Ident(s)
    }

    fn get_numeric(&mut self) -> Token {
        let mut dot = false;
        let s = self.get_while(|c| {
            if c == '.' && !dot {
                dot = true;
                true
            } else {
                c.is_numeric()
            }
        });

        if dot {
            Numeric::Float(s.parse().unwrap()).to_token()
        } else {
            Numeric::Int(s.parse().unwrap()).to_token()
        }
    }

    fn get_string(&mut self) -> Token {
        let mut s = String::new();
    
        while let Some(c) = self.read_next() {
            if c == '\"' {
                self.get_next();
                break;
            } else if c == '\\' {
                self.get_next();
                if let Some(c) = self.read_next() {
                    match c {
                        'n' => s.push('\n'),
                        'r' => s.push('\r'),
                        't' => s.push('\t'),
                        '0' => s.push('\0'),
                        '"' => s.push('"'),
                        '\'' => s.push('\''),
                        '\\' => s.push('\\'),
                        _ => s.push(c),
                    }
                }
            } else {
                s.push(self.get_next().unwrap());
            }
        }
        
        Literal::String(s).to_token()
    }


    fn get_symbol(&mut self) -> Option<Symbol> {
        let c = self.get_next().unwrap();
        // Return if it's a single character symbol
        match c {
            '(' => return Some(Symbol::OpenParen),
            ')' => return Symbol::CloseParen,
            '{' => return Symbol::OpenBrace,
            '}' => return Symbol::CloseBrace,
            '[' => return Symbol::OpenBracket,
            ']' => return Symbol::CloseBracket,
            ',' => return Symbol::Comma,
            ':' => return Symbol::Colon,
            ';' => return Symbol::Semicolon,
            _ => {}
        }

        let c2 = self.read_next();
        if let Some(c2) = c2 {
            let c2 = self.get_next().unwrap();

            match (c, c2) {
                ('+', '+') => return Symbol::Increment,
                ('+', '=') => return Symbol::PlusAssign,
                ('-', '-') => return Symbol::Decrement,
                ('-', '=') => return Symbol::MinusAssign,
                ('*', '=') => return Symbol::MultiplyAssign,
                ('/', '=') => return Symbol::DivideAssign,
                ('%', '=') => return Symbol::ModuloAssign,
                ('=', '=') => return Symbol::Equal,
                ('!', '=') => return Symbol::NotEqual,
                ('<', '=') => return Symbol::LessThanEqual,
                ('>', '=') => return Symbol::GreaterThanEqual,
                ('&', '&') => return Symbol::And,
                ('|', '|') => return Symbol::Or,
                ('&', '=') => return Symbol::BitwiseAnd,
                ('|', '=') => return Symbol::BitwiseOr,
                ('^', '=') => return Symbol::BitwiseXor,
                ('<', '<') => return Symbol::BitwiseLeftShift,
                ('>', '>') => {
                    let c3 = self.read_next();
                    if let Some(c3) = c3 {
                        let c3 = self.get_next().unwrap();
                        if c3 == '>' {
                            return Symbol::BitwiseUnsignedRightShift;
                        }
                    }
                    return Symbol::BitwiseRightShift;
                }
                _ => {}
            }
        }
    }
}


fn x() {
    let mut l = Lexer::new("Hello, world!");
    println!("{}", l.get_while(|c| c.is_alphabetic()));
    

}
