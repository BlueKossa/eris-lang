use super::statements::Statement;

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}
