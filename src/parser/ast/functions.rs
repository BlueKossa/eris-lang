use crate::span::Span;

use super::blocks::Block;
use super::types::Type;

#[derive(Debug, Clone)]
pub struct FnSig {
    pub ret: Type,
    pub args: Vec<(Span, Type)>,
    pub is_variadic: bool,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: Span,
    pub sig: FnSig,
    pub body: Block,
}

//impl std::fmt::Display for FnDecl {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        let mut function = String::new();
//        function.push_str(&format!("fn {}(", self.name));
//        for (i, arg) in self.sig.args.iter().enumerate() {
//            function.push_str(&format!("{}: {}", arg.0, arg.1));
//            if i != self.sig.args.len() - 1 {
//                function.push_str(", ");
//            }
//        }
//        function.push_str(&format!(") -> {}", self.sig.ret));
//        function.push_str(&format!(" {}", self.body));
//        write!(f, "{}", function)
//    }
//}
