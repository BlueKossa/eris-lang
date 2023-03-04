use std::{path::Path, str::CharIndices, fs, iter::Peekable};





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
    Ident(String),
    Literal(Literal),
}

struct Lexer<'a>{
    chars: Peekable<CharIndices<'a>>,
    input: &'a str,
    x_pos: usize,
    y_pos: usize,
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
        if let Some((_, c)) = self.chars.next() {
            self.x_pos += 1;

            if c == '\n' {
                self.x_pos = 0;
                self.y_pos += 1;
            }

            return Some(c);
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

    fn get_number(&mut self) -> Token {
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
            Token::Numeric(Numeric::Float(s.parse().unwrap()))
        } else {
            Token::Numeric(Numeric::Int(s.parse().unwrap()))
        }
    }

    fn get_string(&mut self) -> Token {
        let mut s = String::new();
    
        while let Some(c) = self.read_next() {
            if c == '"' {
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

        Token::Literal(Literal::String(s))
    }


    fn get_symbol(&mut self) -> Symbol {
        
    }
}


fn x() {
    let mut l = Lexer::new("Hello, world!");
    println!("{}", l.get_while(|c| c.is_alphabetic()));
    

}
