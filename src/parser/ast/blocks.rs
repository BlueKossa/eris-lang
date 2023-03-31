use super::statements::Statement;

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}
