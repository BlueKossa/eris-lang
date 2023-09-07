use std::str::FromStr;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Symbol {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,

    Semicolon,
    Newline,

    Colon,
    Comma,
    Dot,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,

    And,
    Pipe,
    Tilde,
    Bang,
    Equal,
    Less,
    Greater,

    At,
    Pound,

    ColonEqual,
    EqualEqual,
    MinusEqual,
    PlusEqual,
    AsteriskEqual,
    SlashEqual,
    PercentEqual,

    BangEqual,
    LessEqual,
    GreaterEqual,
    AndAnd,
    PipePipe,

    ColonColon,
    DotAsterisk,
}

impl FromStr for Symbol {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(Symbol::ParenOpen),
            ")" => Ok(Symbol::ParenClose),
            "{" => Ok(Symbol::BraceOpen),
            "}" => Ok(Symbol::BraceClose),
            "[" => Ok(Symbol::BracketOpen),
            "]" => Ok(Symbol::BracketClose),

            ";" => Ok(Symbol::Semicolon),
            "\n" => Ok(Symbol::Newline),

            ":" => Ok(Symbol::Colon),
            "," => Ok(Symbol::Comma),
            "." => Ok(Symbol::Dot),

            "+" => Ok(Symbol::Plus),
            "-" => Ok(Symbol::Minus),
            "*" => Ok(Symbol::Asterisk),
            "/" => Ok(Symbol::Slash),
            "%" => Ok(Symbol::Percent),

            "&" => Ok(Symbol::And),
            "|" => Ok(Symbol::Pipe),
            "~" => Ok(Symbol::Tilde),
            "!" => Ok(Symbol::Bang),
            "=" => Ok(Symbol::Equal),
            "<" => Ok(Symbol::Less),
            ">" => Ok(Symbol::Greater),

            "@" => Ok(Symbol::At),
            "#" => Ok(Symbol::Pound),

            ":=" => Ok(Symbol::ColonEqual),
            "==" => Ok(Symbol::EqualEqual),
            "+=" => Ok(Symbol::PlusEqual),
            "-=" => Ok(Symbol::MinusEqual),
            "*=" => Ok(Symbol::AsteriskEqual),
            "/=" => Ok(Symbol::SlashEqual),
            "%=" => Ok(Symbol::PercentEqual),

            "!=" => Ok(Symbol::BangEqual),
            "<=" => Ok(Symbol::LessEqual),
            ">=" => Ok(Symbol::GreaterEqual),
            "&&" => Ok(Symbol::AndAnd),
            "||" => Ok(Symbol::PipePipe),

            "::" => Ok(Symbol::ColonColon),
            ".*" => Ok(Symbol::DotAsterisk),

            _ => Err(format!("Invalid symbol: {}", s)),
        }
    }
}
