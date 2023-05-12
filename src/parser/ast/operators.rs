use crate::lexer::symbol::Symbol::*;
use crate::lexer::token::Token;

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    // a + b
    Add,
    // a - b
    Subtract,
    // a * b
    Multiply,
    // a / b
    Divide,
    // a % b
    Modulo,
    // a == b
    Equal,
    // a != b
    NotEqual,
    // a < b
    LessThan,
    // a <= b
    LessThanEqual,
    // a > b
    GreaterThan,
    // a >= b
    GreaterThanEqual,
    // a && b
    And,
    // a || b
    Or,
    // a = b
    Assign,
    // a += b
    AddAssign,
    // a -= b
    SubtractAssign,
    // a *= b
    MultiplyAssign,
    // a /= b
    DivideAssign,
    // a %= b
    ModuloAssign,
}

impl BinaryOp {
    pub fn boolean(&self) -> bool {
        match self {
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanEqual => true,
            _ => false,
        }
    }

    pub fn assignment(&self) -> Option<BinaryOp> {
        match self {
            BinaryOp::AddAssign => Some(BinaryOp::Add),
            BinaryOp::SubtractAssign => Some(BinaryOp::Subtract),
            BinaryOp::MultiplyAssign => Some(BinaryOp::Multiply),
            BinaryOp::DivideAssign => Some(BinaryOp::Divide),
            BinaryOp::ModuloAssign => Some(BinaryOp::Modulo),
            _ => None,
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanEqual => 3,
            BinaryOp::Add | BinaryOp::Subtract => 4,
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 5,
            _ => 0,
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        return match value {
            Token::Symbol(Plus) => Ok(BinaryOp::Add),
            Token::Symbol(Minus) => Ok(BinaryOp::Subtract),
            Token::Symbol(Asterisk) => Ok(BinaryOp::Multiply),
            Token::Symbol(Slash) => Ok(BinaryOp::Divide),
            Token::Symbol(Percent) => Ok(BinaryOp::Modulo),
            Token::Symbol(EqualEqual) => Ok(BinaryOp::Equal),
            Token::Symbol(BangEqual) => Ok(BinaryOp::NotEqual),
            Token::Symbol(Less) => Ok(BinaryOp::LessThan),
            Token::Symbol(LessEqual) => Ok(BinaryOp::LessThanEqual),
            Token::Symbol(Greater) => Ok(BinaryOp::GreaterThan),
            Token::Symbol(GreaterEqual) => Ok(BinaryOp::GreaterThanEqual),
            Token::Symbol(AndAnd) => Ok(BinaryOp::And),
            Token::Symbol(PipePipe) => Ok(BinaryOp::Or),
            Token::Symbol(Equal) => Ok(BinaryOp::Assign),
            Token::Symbol(PlusEqual) => Ok(BinaryOp::AddAssign),
            Token::Symbol(MinusEqual) => Ok(BinaryOp::SubtractAssign),
            Token::Symbol(AsteriskEqual) => Ok(BinaryOp::MultiplyAssign),
            Token::Symbol(SlashEqual) => Ok(BinaryOp::DivideAssign),
            Token::Symbol(PercentEqual) => Ok(BinaryOp::ModuloAssign),
            _ => Err(()),
        };
    }
}
