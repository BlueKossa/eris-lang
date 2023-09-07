use crate::{visitor::visitor_pattern::{ExpressionVisitor, MutVisitorPattern}, parser::ast::{expressions::ExprKind, types::{TypeKind, Type}}};

use super::visitor::SemanticVisitor;

impl<'a> ExpressionVisitor<'a> for SemanticVisitor<'a> {
    type ExprReturnType = Option<Type<'a>>;

    fn visit_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        match expr {
            ExprKind::Binary(_op, lhs, rhs) => {
                let ty1 = self.visit_expr(&mut lhs.kind);
                let ty2 = self.visit_expr(&mut rhs.kind);
                if let (Some(ty1), Some(ty2)) = (ty1.to_owned(), ty2) {
                    self.type_check(&ty1, &ty2);
                }
                ty1
            }
            ExprKind::Unary(_op, expr) => self.visit_expr(&mut expr.kind),
            ExprKind::Literal(lit) => Some(lit.to_ty()),
            ExprKind::Address(expr) => {
                let ty = self.visit_expr(&mut expr.kind).unwrap();
                Some(TypeKind::Ref(ty).into())
            }
            ExprKind::Deref(expr) => {
                let ty = self.visit_expr(&mut expr.kind).unwrap();
                if let TypeKind::Ref(ty) = *ty.kind {
                    Some(ty.into())
                } else {
                    panic!("Expected an address");
                }
            }
            ExprKind::Array(a) => {
                let ty = self.visit_expr(&mut a[0].kind).unwrap();
                for expr in a.iter_mut() {
                    let ty1 = self.visit_expr(&mut expr.kind).unwrap();
                    self.type_check(&ty, &ty1);
                }
                Some(TypeKind::Array(ty, a.len()).into())
            }
            ExprKind::StructInit(name, fields) => {
                let struct_ty = self.structs.get(name).unwrap().to_owned();
                for (field, expr) in fields.iter_mut().enumerate() {
                    let ty1 = self.visit_expr(&mut expr.kind).unwrap();
                    let ty2 = &struct_ty.get(field).unwrap().1;
                    self.type_check(&ty1, ty2);
                }
                Some(TypeKind::Struct(name).into())
            }
            ExprKind::FieldAccess(struct_, field) => {
                let struct_ty = self.visit_expr(&mut struct_.kind).unwrap();
                if let TypeKind::Struct(name) = *struct_ty.kind {
                    let struct_ty = self.structs.get(name).unwrap().to_owned();
                    let field_ty = &struct_ty.iter().find(|(f, _)| f == field).unwrap().1;
                    Some(field_ty.to_owned())
                } else if let TypeKind::Ref(ty) = *struct_ty.kind {
                    if let TypeKind::Struct(name) = *ty.kind {
                        let struct_ty = self.structs.get(name).unwrap().to_owned();
                        let field_ty = &struct_ty.iter().find(|(f, _)| f == field).unwrap().1;
                        Some(field_ty.to_owned())
                    } else {
                        panic!("Expected struct type");
                    }
                } else {
                    panic!("Expected struct type");
                }
            }
            ExprKind::ArrayIndex(array, _index) => {
                //TODO
                if let TypeKind::Array(ty, _) = *self.visit_expr(&mut array.kind).unwrap().kind {
                    Some(ty)
                } else {
                    panic!("Expected array type");
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
                let ty1 = self.visit_expr(&mut var.kind).unwrap();
                let ty2 = self.visit_expr(&mut rhs.kind).unwrap();
                self.type_check(&ty1, &ty2);
                None
            }
            ExprKind::Call(func, params) => {
                let res = self.fn_decls.get(func);
                if res.is_none() {
                    return Some(TypeKind::Void.into());
                }
                let (param_types, fn_type) = res.unwrap().to_owned();
                for (i, expr) in params.iter_mut().enumerate() {
                    let ty1 = self.visit_expr(&mut expr.kind).unwrap();
                    let ty2 = param_types.get(i).unwrap();
                    self.type_check(&ty1, ty2);
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

    fn visit_binary(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_unary(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_literal(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_address_of(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_deref(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_array(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_struct(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_field(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_index(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_call(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_if(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_loop(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_assign(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_var(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_break(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }

    fn visit_return(&mut self, expr: &mut crate::parser::ast::expressions::ExprKind<'a>) -> Self::ExprReturnType {
        todo!()
    }
}