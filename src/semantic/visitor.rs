use std::collections::HashMap;

use crate::parser::ast::expressions::ExprKind;
use crate::parser::ast::functions::FnDecl;
use crate::parser::ast::items::Item;
use crate::parser::ast::items::ItemKind::{Function, Struct, Constant};
use crate::parser::ast::locals::Local;
use crate::parser::ast::statements::Statement;
use crate::parser::ast::types::Type;
use crate::{visitor::visitor_pattern::MutVisitorPattern, parser::ast::blocks::Block};
use crate::visitor::chainmap::ChainMap;

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
        if let (Some(ty1), Some(ty2)) = (ty, local.ty) {
            if ty1 != ty2 {
                type_mismatch(&ty1, &ty2);
            }
        } else if let (Some(_), None) = (ty, local.ty) {
            local.ty = ty;
        }
        self.values.insert(local.ident, local.ty.unwrap());
        None
    }

    fn traverse_expr(&mut self, expr: &mut ExprKind<'a> ) -> Self::ReturnType {
        match expr {
            ExprKind::Binary(_op, lhs, rhs) => {
                let ty1 = self.traverse_expr(&mut lhs.kind);
                let ty2 = self.traverse_expr(&mut rhs.kind);
                if let (Some(ty1), Some(ty2)) = (ty1, ty2) {
                    if ty1 != ty2 {
                        type_mismatch(&ty1, &ty2)
                    }
                }
                ty1
            }
            ExprKind::Unary(_op, expr) => {
                self.traverse_expr(&mut expr.kind)
            }
            ExprKind::Literal(lit) => {
                Some(lit.to_ty())
            }
            ExprKind::StructInit(name, fields) => {
                let struct_ty = *self.structs.get(name).unwrap().to_owned();
                for (field, expr) in fields.iter_mut().enumerate() {
                    let ty1 = self.traverse_expr(&mut expr.kind).unwrap();
                    let ty2 = struct_ty.get(field).unwrap().1;
                    if ty1 != ty2 {
                        type_mismatch(&ty1, &ty2);
                    }
                }
                Some(Type::Struct(name))
            }
            ExprKind::FieldAccess(struct_, field) => {
                let struct_ty = self.traverse_expr(&mut struct_.kind).unwrap();
                if let Type::Struct(name) = struct_ty {
                    let struct_ty = *self.structs.get(name).unwrap().to_owned();
                    let field_ty = struct_ty.iter().find(|(f, _)| f == field).unwrap().1;
                    Some(field_ty)
                } else {
                    panic!("Expected struct type, found {:?}", struct_ty);
                }
            }
            ExprKind::MethodCall(_, _, _) => todo!(),
            ExprKind::If(_, _) => todo!(),
            ExprKind::While(_, _) => todo!(),
            ExprKind::Assign(var, rhs) => {
                let ty1 = self.traverse_expr(&mut var.kind).unwrap();
                let ty2 = self.traverse_expr(&mut rhs.kind).unwrap();
                if ty1 != ty2 {
                    type_mismatch(&ty1, &ty2);
                }
                None
            }
            ExprKind::Call(func, params) => {
                let (param_types, fn_type) = *self.fn_decls.get(func).unwrap().to_owned();
                for (i, expr) in params.iter_mut().enumerate() {
                    let ty1 = self.traverse_expr(&mut expr.kind).unwrap();
                    let ty2 = param_types.get(i).unwrap();
                    if &ty1 != ty2 {
                        type_mismatch(&ty1, &ty2);
                    }
                }
                Some(fn_type.to_owned())
            }
            ExprKind::Var(v) => {
                Some(self.values.get(v).unwrap().to_owned())
            },
            ExprKind::Return(_) => None,

        }
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ReturnType {
        match &mut item.kind {
            Function(decl) => {
                self.traverse_function(decl)
            },
            Struct(s) => {
                let fields = s.fields.iter().map(|f| (f.name, f.ty)).collect::<Vec<_>>();
                self.structs.insert(s.name, Box::new(fields));
                None
            }
            Constant(_, _, _) => todo!(),
        }
    }

    fn traverse_function(&mut self, function: &mut FnDecl<'a>) -> Self::ReturnType {
        let sig = &mut function.sig;
        let args = sig.args.iter().map(|(_name, ty)| *ty).collect::<Vec<_>>();
        self.fn_decls.insert(function.name, Box::new((args, sig.ret)));
        for (name, ty) in sig.args.iter() {
            self.values.insert(name, *ty);
        }
        self.traverse_block(&mut function.body);
        None
    }
}

impl <'a> SemanticVisitor<'a> {
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

