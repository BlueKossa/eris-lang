pub enum UnaryOp {
    Negate,
    Not,
}

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
}
