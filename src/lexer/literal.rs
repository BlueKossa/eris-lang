

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Number<'a> {
    Integer(&'a str),
    Float(&'a str),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal<'a> {
    String(&'a str),
    Number(Number<'a>),
    Boolean(bool),
}



