use crate::span::Span;

use super::types::Type;

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Span,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Span,
    pub fields: Vec<Field>,
}

//impl std::fmt::Display for Struct {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        let mut structure = String::new();
//        structure.push_str(&format!("struct {} {{\n", self.name));
//        for field in &self.fields {
//            structure.push_str(&format!("  {}: {},\n", field.name, field.ty));
//        }
//        structure.push_str("}");
//        write!(f, "{}", structure)
//    }
//}
