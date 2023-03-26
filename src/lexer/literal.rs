

#[derive(Debug, PartialEq)]
pub enum Number<'a> {
    Integer(&'a str),
    Float(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Number(Number<'a>),
    Boolean(bool),
}



