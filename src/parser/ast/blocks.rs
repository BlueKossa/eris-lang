use super::statements::Statement;

#[derive(Debug, Clone)]
pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}


// Debug
impl<'a> std::fmt::Display for Block<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut statements = String::new();
        statements.push_str("{\n");
        for statement in &self.statements {
            statements.push_str(&format!("  {}\n", statement));
        }
        statements.push_str("}");
        write!(f, "{}", statements)
    }
}
