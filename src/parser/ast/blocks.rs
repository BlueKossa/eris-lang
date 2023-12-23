use super::statements::Statement;

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

//impl std::fmt::Display for Block {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        let mut statements = String::new();
//        statements.push_str("{\n");
//        for statement in &self.statements {
//            statements.push_str(&format!("  {}\n", statement));
//        }
//        statements.push_str("}");
//        write!(f, "{}", statements)
//    }
//}
