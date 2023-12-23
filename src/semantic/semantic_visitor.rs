use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::functions::{FnDecl, FnSig};
use crate::parser::ast::items::{Item, ItemKind};
use crate::parser::ast::locals::Local;
use crate::parser::ast::statements::Statement;
use crate::parser::ast::types::{Type, TypeKind};
use crate::visitor::chainmap::ChainMap;
use crate::visitor::visitor_pattern::ExpressionVisitor;
use crate::{parser::ast::blocks::Block, visitor::visitor_pattern::MutVisitorPattern};

pub struct SemanticVisitor {
    pub(super) values: ChainMap<Rc<str>, Type>,
    pub(crate) fn_decls: HashMap<Rc<str>, FnSig>,
    pub(crate) structs: HashMap<Rc<str>, Vec<(Rc<str>, Type)>>,
    pub(crate) source: String,
}

#[inline(always)]
fn type_mismatch(t1: &TypeKind, t2: &TypeKind) {
    panic!("Type mismatch: {:?} != {:?}", t1, t2);
}

impl<'a> MutVisitorPattern<'a> for SemanticVisitor {
    type ReturnType = Option<Type>;

    fn traverse_block(&mut self, block: &mut Block) -> Self::ReturnType {
        self.values.insert_map();
        for stmt in block.statements.iter_mut() {
            self.traverse_statement(stmt);
        }
        self.values.pop_map();
        None
    }

    fn traverse_statement(&mut self, statement: &mut Statement) -> Self::ReturnType {
        match statement {
            Statement::Local(local) => self.traverse_local(local),
            Statement::Expression(expr) => self.visit_expr(&mut expr.kind),
            Statement::Item(item) => self.traverse_item(item),
        }
    }

    fn traverse_local(&mut self, local: &mut Local) -> Self::ReturnType {
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
        let ident = local.ident.data(&self.source);
        self.values
            .insert(ident.into(), local.ty.as_ref().unwrap().to_owned());
        None
    }

    fn traverse_item(&mut self, item: &mut Item) -> Self::ReturnType {
        use crate::parser::ast::items::ItemKind as IK;
        match &mut item.kind {
            IK::Function(decl) => self.traverse_function(decl),
            IK::Struct(s) => {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| (f.name.data(&self.source).into(), f.ty.to_owned()))
                    .collect::<Vec<_>>();
                let name = s.name.data(&self.source);
                self.structs.insert(name.into(), fields);
                None
            }
            IK::Constant(_, _, _) => todo!(),
            IK::Foreign(b) => self.traverse_block(b),
        }
    }

    fn traverse_function<'b>(&mut self, function: &mut FnDecl) -> Self::ReturnType {
        let sig = &mut function.sig;
        if self.fn_decls.get(function.name.data(&self.source)).is_none() {
            let name: Rc<str> = function.name.data(&self.source).into();
            self.fn_decls
                .insert(name, sig.clone());
        }
        for (name, ty) in sig.args.iter() {
            let name: Rc<str> = name.data(&self.source).into();
            self.values.insert(name, ty.to_owned());
        }
        self.traverse_block(&mut function.body);
        None
    }
}

impl SemanticVisitor {
    pub fn new() -> Self {
        Self {
            values: ChainMap::new(),
            fn_decls: HashMap::new(),
            structs: HashMap::new(),
            source: String::new(),
        }
    }

    pub fn run(&mut self, entry: &mut Block, source: String) {
        self.source = source;
        self.run_light(entry);
        self.traverse_block(entry);
    }

    pub fn run_light(&mut self, entry: &Block) {
        for statement in entry.statements.iter() {
            match statement {
                Statement::Item(item) => {
                    match item.kind {
                        ItemKind::Function(ref func) => {
                            let sig = &func.sig;
                            let name = func.name.data(&self.source);
                            self.fn_decls.insert(name.into(), sig.clone());
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
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
