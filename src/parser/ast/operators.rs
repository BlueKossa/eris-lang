use crate::lexer::symbol::Symbol::*;
use crate::lexer::token::{Token, TokenKind};

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

impl TryFrom<TokenKind> for BinaryOp {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        return match value {
            TokenKind::Symbol(Plus) => Ok(BinaryOp::Add),
            TokenKind::Symbol(Minus) => Ok(BinaryOp::Subtract),
            TokenKind::Symbol(Asterisk) => Ok(BinaryOp::Multiply),
            TokenKind::Symbol(Slash) => Ok(BinaryOp::Divide),
            TokenKind::Symbol(Percent) => Ok(BinaryOp::Modulo),
            TokenKind::Symbol(EqualEqual) => Ok(BinaryOp::Equal),
            TokenKind::Symbol(BangEqual) => Ok(BinaryOp::NotEqual),
            TokenKind::Symbol(Less) => Ok(BinaryOp::LessThan),
            TokenKind::Symbol(LessEqual) => Ok(BinaryOp::LessThanEqual),
            TokenKind::Symbol(Greater) => Ok(BinaryOp::GreaterThan),
            TokenKind::Symbol(GreaterEqual) => Ok(BinaryOp::GreaterThanEqual),
            TokenKind::Symbol(AndAnd) => Ok(BinaryOp::And),
            TokenKind::Symbol(PipePipe) => Ok(BinaryOp::Or),
            TokenKind::Symbol(Equal) => Ok(BinaryOp::Assign),
            TokenKind::Symbol(PlusEqual) => Ok(BinaryOp::AddAssign),
            TokenKind::Symbol(MinusEqual) => Ok(BinaryOp::SubtractAssign),
            TokenKind::Symbol(AsteriskEqual) => Ok(BinaryOp::MultiplyAssign),
            TokenKind::Symbol(SlashEqual) => Ok(BinaryOp::DivideAssign),
            TokenKind::Symbol(PercentEqual) => Ok(BinaryOp::ModuloAssign),
            _ => Err(()),
        };
    }
}

//impl std::fmt::Display for BinaryOp {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        let op = match self {
//            BinaryOp::Add => "+",
//            BinaryOp::Subtract => "-",
//            BinaryOp::Multiply => "*",
//            BinaryOp::Divide => "/",
//            BinaryOp::Modulo => "%",
//            BinaryOp::Equal => "==",
//            BinaryOp::NotEqual => "!=",
//            BinaryOp::LessThan => "<",
//            BinaryOp::LessThanEqual => "<=",
//            BinaryOp::GreaterThan => ">",
//            BinaryOp::GreaterThanEqual => ">=",
//            BinaryOp::And => "&&",
//            BinaryOp::Or => "||",
//            BinaryOp::Assign => "=",
//            BinaryOp::AddAssign => "+=",
//            BinaryOp::SubtractAssign => "-=",
//            BinaryOp::MultiplyAssign => "*=",
//            BinaryOp::DivideAssign => "/=",
//            BinaryOp::ModuloAssign => "%=",
//        };
//        write!(f, "{}", op)
//    }
//}
//
//impl std::fmt::Display for UnaryOp {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match self {
//            UnaryOp::Negate => write!(f, "-"),
//            UnaryOp::Not => write!(f, "!"),
//        }
//    }
//}
