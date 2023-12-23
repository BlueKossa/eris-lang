#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Number {
    Integer {
        signed: bool,
        size: usize,
    },
    Float {
        size: usize,
    }
}

impl Number {
    pub fn new_integer(signed: bool, size: usize) -> Number {
        Number::Integer { signed, size }
    }

    pub fn default_integer() -> Number {
        Number::Integer {
            signed: false,
            size: 64,
        }
    }

    pub fn new_float(size: usize) -> Number {
        Number::Float { size }
    }

    pub fn default_float() -> Number {
        Number::Float { size: 64 }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal {
    String,
    Number(Number),
    Boolean,
}
