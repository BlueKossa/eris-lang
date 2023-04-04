
#[derive(Debug, Clone)]
pub enum Keyword {
    Decl,
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
    Class,
    Union,
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "decl" => Some(Keyword::Decl),
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
            "class" => Some(Keyword::Class),
            "union" => Some(Keyword::Union),
            _ => None,
        }
    }
}