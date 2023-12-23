use super::expressions::Expr;
use super::items::Item;
use super::locals::Local;

#[derive(Debug, Clone)]
pub enum Statement {
    Local(Local),
    Item(Item),
    Expression(Expr),
}

//impl std::fmt::Display for Statement {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        match self {
//            Statement::Local(local) => write!(f, "{}", local),
//            Statement::Item(item) => write!(f, "{}", item),
//            Statement::Expression(expr) => write!(f, "{}", expr),
//        }
//    }
//}
