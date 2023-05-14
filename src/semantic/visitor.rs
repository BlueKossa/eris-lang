use std::collections::HashMap;

use crate::parser::ast::expressions::ExprKind;
use crate::parser::ast::functions::FnDecl;
use crate::parser::ast::items::Item;
use crate::parser::ast::items::ItemKind::{Constant, Function, Struct};
use crate::parser::ast::locals::Local;
use crate::parser::ast::statements::Statement;
use crate::parser::ast::types::{Type, TypeKind};
use crate::visitor::chainmap::ChainMap;
use crate::{parser::ast::blocks::Block, visitor::visitor_pattern::MutVisitorPattern};

pub struct SemanticVisitor<'a> {
    values: ChainMap<&'a str, Type<'a>>,
    pub(crate) fn_decls: HashMap<&'a str, Box<(Vec<Type<'a>>, Type<'a>)>>,
    pub(crate) structs: HashMap<&'a str, Box<Vec<(&'a str, Type<'a>)>>>,
}

#[inline(always)]
fn type_mismatch<'a>(t1: &Type<'a>, t2: &Type<'a>) {
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
            Statement::Expression(expr) => self.traverse_expr(&mut expr.kind),
            Statement::Item(item) => self.traverse_item(item),
        }
    }

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ReturnType {
        let ty = if let Some(expr) = &mut local.value {
            self.traverse_expr(&mut expr.kind)
        } else {
            None
        };
        if let (Some(ty1), Some(ty2)) = (&ty, &local.ty) {
            if ty1 != ty2 {
                type_mismatch(&ty1, &ty2);
            }
        } else if let (Some(t), None) = (ty, &local.ty) {
            local.ty = Some(t);
        }
        self.values
            .insert(local.ident, local.ty.as_ref().unwrap().to_owned());
        None
    }

    fn traverse_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ReturnType {
        match expr {
            ExprKind::Binary(_op, lhs, rhs) => {
                let ty1 = self.traverse_expr(&mut lhs.kind);
                let ty2 = self.traverse_expr(&mut rhs.kind);
                if let (Some(ty1), Some(ty2)) = (ty1.to_owned(), ty2) {
                    if ty1 != ty2 {
                        type_mismatch(&ty1, &ty2)
                    }
                }
                ty1
            }
            ExprKind::Unary(_op, expr) => self.traverse_expr(&mut expr.kind),
            ExprKind::Literal(lit) => Some(lit.to_ty()),
            ExprKind::Array(a) => {
                let ty = self.traverse_expr(&mut a[0].kind).unwrap();
                for expr in a.iter_mut() {
                    let ty1 = self.traverse_expr(&mut expr.kind).unwrap();
                    if ty1 != ty {
                        type_mismatch(&ty1, &ty);
                    }
                }
                Some(TypeKind::Array(ty, a.len()).into())
            }
            ExprKind::StructInit(name, fields) => {
                let struct_ty = *self.structs.get(name).unwrap().to_owned();
                for (field, expr) in fields.iter_mut().enumerate() {
                    let ty1 = self.traverse_expr(&mut expr.kind).unwrap();
                    let ty2 = &struct_ty.get(field).unwrap().1;
                    if ty1 != *ty2 {
                        type_mismatch(&ty1, &ty2);
                    }
                }
                Some(TypeKind::Struct(name).into())
            }
            ExprKind::FieldAccess(struct_, field) => {
                let struct_ty = self.traverse_expr(&mut struct_.kind).unwrap();
                if let TypeKind::Struct(name) = *struct_ty.kind {
                    let struct_ty = *self.structs.get(name).unwrap().to_owned();
                    let field_ty = &struct_ty.iter().find(|(f, _)| f == field).unwrap().1;
                    Some(field_ty.to_owned())
                } else {
                    panic!("Expected struct type, found {:?}", struct_ty);
                }
            }
            ExprKind::MethodCall(_, _, _) => todo!(),
            ExprKind::If(_cond, body) => {
                //TODO
                self.traverse_block(body);
                None
            }
            ExprKind::Loop(_cond, body) => {
                //TODO: Check if loop is infinite
                self.traverse_block(body);
                None
            }
            ExprKind::Assign(var, rhs) => {
                let ty1 = self.traverse_expr(&mut var.kind).unwrap();
                let ty2 = self.traverse_expr(&mut rhs.kind).unwrap();
                if ty1 != ty2 {
                    type_mismatch(&ty1, &ty2);
                }
                None
            }
            ExprKind::Call(func, params) => {
                let res = self.fn_decls.get(func);
                if res.is_none() {
                    return Some(TypeKind::Void.into());
                }
                let (param_types, fn_type) = *res.unwrap().to_owned();
                for (i, expr) in params.iter_mut().enumerate() {
                    let ty1 = self.traverse_expr(&mut expr.kind).unwrap();
                    let ty2 = param_types.get(i).unwrap();
                    if &ty1 != ty2 {
                        type_mismatch(&ty1, &ty2);
                    }
                }
                Some(fn_type.to_owned())
            }
            ExprKind::Var(v) => Some(self.values.get(v).unwrap().to_owned()),
            ExprKind::Break => {
                //TODO: Check if break is inside a loop 
                None
            }
            ExprKind::Return(_) => None,
        }
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ReturnType {
        match &mut item.kind {
            Function(decl) => self.traverse_function(decl),
            Struct(s) => {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| (f.name, f.ty.to_owned()))
                    .collect::<Vec<_>>();
                self.structs.insert(s.name, Box::new(fields));
                None
            }
            Constant(_, _, _) => todo!(),
        }
    }

    fn traverse_function(&mut self, function: &mut FnDecl<'a>) -> Self::ReturnType {
        let sig = &mut function.sig;
        let args = sig
            .args
            .iter()
            .map(|(_name, ty)| ty.to_owned())
            .collect::<Vec<_>>();
        self.fn_decls
            .insert(function.name, Box::new((args, sig.ret.to_owned())));
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
        drop(entry);
    }
}
