#[derive(Debug, Clone)]
pub enum Keyword {
    Require,
    Return,
    Mut,
    If,
    Else,
    While,
    For,
    Loop,
    Break,
    Continue,
    Match,
    Struct,
    Union,
    Foreign,
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "require" => Some(Keyword::Require),
            "return" => Some(Keyword::Return),
            "mut" => Some(Keyword::Mut),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "while" => Some(Keyword::While),
            "for" => Some(Keyword::For),
            "loop" => Some(Keyword::Loop),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            "match" => Some(Keyword::Match),
            "struct" => Some(Keyword::Struct),
            "union" => Some(Keyword::Union),
            "foreign" => Some(Keyword::Foreign),
            _ => None,
        }
    }
}
