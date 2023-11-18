use std::collections::HashMap;

use crate::parser::ast::functions::{FnDecl, FnSig};
use crate::parser::ast::items::Item;
use crate::parser::ast::locals::Local;
use crate::parser::ast::statements::Statement;
use crate::parser::ast::types::{Type, TypeKind};
use crate::visitor::chainmap::ChainMap;
use crate::visitor::visitor_pattern::ExpressionVisitor;
use crate::{parser::ast::blocks::Block, visitor::visitor_pattern::MutVisitorPattern};

pub struct SemanticVisitor<'a> {
    pub(super) values: ChainMap<&'a str, Type<'a>>,
    pub(crate) fn_decls: HashMap<&'a str, FnSig<'a>>,
    pub(crate) structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
}

#[inline(always)]
fn type_mismatch<'a>(t1: &TypeKind<'a>, t2: &TypeKind<'a>) {
    panic!("Type mismatch: {:?} != {:?}", t1, t2);
}

impl<'a> MutVisitorPattern<'a> for SemanticVisitor<'a> {
    type ReturnType = Option<Type<'a>>;

    fn traverse_block(&mut self, block: &mut Block<'a>) -> Self::ReturnType {
        self.values.insert_map();
        for stmt in block.statements.iter_mut() {
            self.traverse_statement(stmt);
        }
        self.values.pop_map();
        None
    }

    fn traverse_statement(&mut self, statement: &mut Statement<'a>) -> Self::ReturnType {
        match statement {
            Statement::Local(local) => self.traverse_local(local),
            Statement::Expression(expr) => self.visit_expr(&mut expr.kind),
            Statement::Item(item) => self.traverse_item(item),
        }
    }

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ReturnType {
        let ty = if let Some(expr) = &mut local.value {
            self.visit_expr(&mut expr.kind)
        } else {
            None
        };
        if let (Some(ty1), Some(ty2)) = (&ty, &local.ty) {
            self.type_check(ty1, ty2);
        } else if let (Some(t), None) = (ty, &local.ty) {
            local.ty = Some(t);
        }
        self.values
            .insert(local.ident, local.ty.as_ref().unwrap().to_owned());
        None
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ReturnType {
        use crate::parser::ast::items::ItemKind as IK;
        match &mut item.kind {
            IK::Function(decl) => self.traverse_function(decl),
            IK::Struct(s) => {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| (f.name, f.ty.to_owned()))
                    .collect::<Vec<_>>();
                self.structs.insert(s.name, fields);
                None
            }
            IK::Constant(_, _, _) => todo!(),
            IK::Foreign(b) => self.traverse_block(b),
        }
    }

    fn traverse_function<'b>(&mut self, function: &mut FnDecl<'a>) -> Self::ReturnType {
        let sig = &mut function.sig;
        println!("sig: {:?}", sig);
        self.fn_decls
            .insert(function.name, sig.clone());
        for (name, ty) in sig.args.iter() {
            self.values.insert(name, ty.to_owned());
        }
        self.traverse_block(&mut function.body);
        None
    }
}

impl<'a> SemanticVisitor<'a> {
    pub fn new() -> Self {
        Self {
            values: ChainMap::new(),
            fn_decls: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn run(&mut self, entry: &mut Block<'a>) {
        self.traverse_block(entry);
    }

    pub(super) fn type_check(&self, a: &Type, b: &Type) {
        let _ty1 = a;
        let _ty2 = b;
        // TODO: Review
        //loop {
        //    match (*ty1.kind.to_owned(), *ty2.kind.to_owned()) {
        //        (TypeKind::Ref(t1), TypeKind::Ref(t2)) => {
        //            ty1 = &t1;
        //            ty2 = &t2;
        //        }
        //        (TypeKind::Ref(ty1), _) => self.type_check(&ty1, ty2),
        //        (_, TypeKind::Ref(ty2)) => self.type_check(ty1, &ty2),
        //        (t1, t2) => {
        //            if t1 != t2 {
        //                type_mismatch(&t1, &t2);
        //            }
        //        }
        //    }
        //}
    }
}
