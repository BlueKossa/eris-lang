use super::types::Type;

#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone)]
pub struct Struct<'a> {
    pub name: &'a str,
    pub fields: Vec<Field<'a>>,
}

impl<'a> std::fmt::Display for Struct<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut structure = String::new();
        structure.push_str(&format!("struct {} {{\n", self.name));
        for field in &self.fields {
            structure.push_str(&format!("  {}: {},\n", field.name, field.ty));
        }
        structure.push_str("}");
        write!(f, "{}", structure)
    }
}
