use inkwell::{
    attributes::{Attribute, AttributeLoc},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum},
};

use super::visitor::{CodeGenResult, CodeGenVisitor};
use crate::{
    parser::ast::{
        expressions::ExprKind,
        literals::LiteralKind,
        operators::{BinaryOp, UnaryOp},
    },
    visitor::visitor_pattern::{ExpressionVisitor, MutVisitorPattern},
};

macro_rules! create_local_tuple {
    ($enum_kind:ident, $expr:ident, $($name:ident),*) => {
        let ($($name,)*) = if let ExprKind::$enum_kind($($name,)*) = $expr {
            ($($name,)*)
        } else {
            unreachable!()
        };
    };
}

impl<'a> ExpressionVisitor<'a> for CodeGenVisitor<'a> {
    type ExprReturnType = Option<CodeGenResult<'a>>;

    fn visit_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        match expr {
            ExprKind::Binary(_, _, _) => self.visit_binary(expr),
            ExprKind::Unary(_, _) => self.visit_unary(expr),
            ExprKind::Literal(_) => self.visit_literal(expr),
            ExprKind::Address(_) => self.visit_address_of(expr),
            ExprKind::Deref(_) => self.visit_deref(expr),
            ExprKind::Cast(_, _) => self.visit_cast(expr),
            ExprKind::Array(_) => self.visit_array(expr),
            ExprKind::StructInit(_, _) => self.visit_struct(expr),
            ExprKind::FieldAccess(_, _) => self.visit_field(expr),
            ExprKind::ArrayIndex(_, _) => self.visit_index(expr),
            ExprKind::Call(_, _) => self.visit_call(expr),
            ExprKind::MethodCall(_, _, _) => self.visit_call(expr),
            ExprKind::If(_, _) => self.visit_if(expr),
            ExprKind::Loop(_, _) => self.visit_loop(expr),
            ExprKind::Assign(_, _) => self.visit_assign(expr),
            ExprKind::Var(_) => self.visit_var(expr),
            ExprKind::Break => self.visit_break(expr),
            ExprKind::Return(_) => self.visit_return(expr),
        }
    }

    fn visit_binary(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        use inkwell::{FloatPredicate as FP, IntPredicate as IP};
        create_local_tuple!(Binary, expr, op, lhs, rhs);
        let lhs = self.visit_expr(&mut lhs.kind).unwrap();
        let rhs = self.visit_expr(&mut rhs.kind).unwrap();
        let mut lhs_val = lhs.value;
        let mut rhs_val = rhs.value;
        if let BasicValueEnum::PointerValue(ptr) = lhs_val {
            lhs_val = self.builder.build_load(ptr, "load").unwrap();
        }
        if let BasicValueEnum::PointerValue(ptr) = rhs_val {
            rhs_val = self.builder.build_load(ptr, "load").unwrap();
        }
        let ty = lhs.ty.unwrap();
        let val: BasicValueEnum = match (lhs_val, rhs_val) {
            (BasicValueEnum::IntValue(a), BasicValueEnum::IntValue(b)) => {
                let res = match op {
                    BinaryOp::Add => self.builder.build_int_add(a, b, "add"),
                    BinaryOp::Subtract => self.builder.build_int_sub(a, b, "sub"),
                    BinaryOp::Multiply => self.builder.build_int_mul(a, b, "mul"),
                    BinaryOp::Divide => self.builder.build_int_signed_div(a, b, "div"),
                    BinaryOp::Modulo => self.builder.build_int_signed_rem(a, b, "rem"),
                    BinaryOp::Equal => self.builder.build_int_compare(IP::EQ, a, b, "eq"),
                    BinaryOp::NotEqual => self.builder.build_int_compare(IP::NE, a, b, "ne"),
                    BinaryOp::LessThan => self.builder.build_int_compare(IP::SLT, a, b, "lt"),
                    BinaryOp::LessThanEqual => self.builder.build_int_compare(IP::SLE, a, b, "le"),
                    BinaryOp::GreaterThan => self.builder.build_int_compare(IP::SGT, a, b, "gt"),
                    BinaryOp::GreaterThanEqual => {
                        self.builder.build_int_compare(IP::SGE, a, b, "ge")
                    }
                    o => unimplemented!("{:?}", o),
                }
                .unwrap();
                res.into()
            }
            (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => {
                let res: BasicValueEnum = match op {
                    BinaryOp::Add => self.builder.build_float_add(a, b, "add").unwrap().into(),
                    BinaryOp::Subtract => self.builder.build_float_sub(a, b, "sub").unwrap().into(),
                    BinaryOp::Multiply => self.builder.build_float_mul(a, b, "mul").unwrap().into(),
                    BinaryOp::Divide => self.builder.build_float_div(a, b, "div").unwrap().into(),
                    BinaryOp::Modulo => self.builder.build_float_rem(a, b, "rem").unwrap().into(),
                    BinaryOp::Equal => self
                        .builder
                        .build_float_compare(FP::OEQ, a, b, "eq")
                        .unwrap()
                        .into(),
                    BinaryOp::NotEqual => self
                        .builder
                        .build_float_compare(FP::ONE, a, b, "ne")
                        .unwrap()
                        .into(),
                    BinaryOp::LessThan => self
                        .builder
                        .build_float_compare(FP::OLT, a, b, "lt")
                        .unwrap()
                        .into(),

                    BinaryOp::LessThanEqual => self
                        .builder
                        .build_float_compare(FP::OLE, a, b, "le")
                        .unwrap()
                        .into(),

                    BinaryOp::GreaterThan => self
                        .builder
                        .build_float_compare(FP::OGT, a, b, "gt")
                        .unwrap()
                        .into(),

                    BinaryOp::GreaterThanEqual => self
                        .builder
                        .build_float_compare(FP::OGE, a, b, "ge")
                        .unwrap()
                        .into(),

                    o => unimplemented!("{:?}", o),
                };
                res
            }
            _ => unimplemented!(),
        };
        Some(CodeGenResult {
            value: val,
            ty: Some(ty),
        })
    }

    fn visit_unary(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Unary, expr, op, expr);
        let expr = self.visit_expr(&mut expr.kind).unwrap();
        let mut expr_val = expr.value;
        if let BasicValueEnum::PointerValue(ptr) = expr_val {
            expr_val = self.builder.build_load(ptr, "load").unwrap();
        }
        let ty = expr.ty.unwrap();
        let val: BasicValueEnum = match expr_val {
            BasicValueEnum::IntValue(a) => {
                let res = match op {
                    UnaryOp::Negate => self.builder.build_int_neg(a, "neg"),
                    UnaryOp::Not => self.builder.build_not(a, "not"),
                }
                .unwrap();
                res.into()
            }
            BasicValueEnum::FloatValue(a) => {
                let res = match op {
                    UnaryOp::Negate => self.builder.build_float_neg(a, "neg"),
                    o => unimplemented!("{:?}", o),
                }
                .unwrap();
                res.into()
            }
            _ => unimplemented!(),
        };
        Some(CodeGenResult {
            value: val,
            ty: Some(ty),
        })
    }

    fn visit_literal(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Literal, expr, lit);
        let val: BasicValueEnum = match lit.kind {
            LiteralKind::Int(i) => self.context.i32_type().const_int(i as u64, false).into(),
            LiteralKind::Float(f) => self.context.f64_type().const_float(f).into(),
            LiteralKind::String(s) => {
                let mut esc = false;
                let bytes = &s[1..s.len() - 1];
                let mut s = String::new();
                for c in bytes.chars() {
                    if esc {
                        match c {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            ch => s.push(ch),
                        }
                        esc = false;
                    } else if c == '\\' {
                        esc = true;
                    } else {
                        s.push(c);
                    }
                }
                let string = self.builder.build_global_string_ptr(&s, "string").unwrap();
                let ptr = string.as_pointer_value();
                return Some(CodeGenResult {
                    value: ptr.into(),
                    ty: Some(ptr.get_type().into()),
                });
            }
            LiteralKind::Bool(b) => self.context.bool_type().const_int(b as u64, false).into(),
        };
        return Some(CodeGenResult {
            value: val,
            ty: Some(val.get_type()),
        });
    }

    fn visit_address_of(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Address, expr, expr);
        if let ExprKind::Var(_var) = *expr.kind {
            return self.visit_expr(&mut expr.kind);
        }
        let expr = self.visit_expr(&mut expr.kind).unwrap();
        let ty = expr.ty.unwrap();
        let val = expr.value;
        let ptr = val.into_pointer_value();
        Some(CodeGenResult {
            value: ptr.into(),
            ty: Some(ty),
        })
    }

    fn visit_deref(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Deref, expr, expr);
        let expr = self.visit_expr(&mut expr.kind).unwrap();
        let ty = expr.ty.unwrap();
        let val = expr.value;
        let ptr = if let BasicValueEnum::PointerValue(ptr) = val {
            ptr
        } else {
            panic!("Expected pointer type");
        };
        let val = self.builder.build_load(ptr, "load").unwrap();
        Some(CodeGenResult {
            value: val,
            ty: Some(ty),
        })
    }

    fn visit_cast(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Cast, expr, expr, ty);

        let expr = self.visit_expr(&mut expr.kind).unwrap();
        let val = expr.value;
        let ty = self.to_llvm_type(ty).into_int_type();

        let val = if let BasicValueEnum::PointerValue(ptr) = val {
            self.builder.build_load(ptr, "load").unwrap()
        } else {
            val
        };
        let val = self
            .builder
            .build_int_cast(val.into_int_value(), ty, "cast")
            .unwrap();
        Some(CodeGenResult {
            value: val.into(),
            ty: Some(ty.into()),
        })
    }

    fn visit_array(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Array, expr, exprs);
        // Constant arrays
        // How it SHOULD be done:
        // %1 = alloca i32, align 4
        // %2 = alloca i32, align 4
        // %3 = alloca [2 x i32], align 4
        // store i32 0, ptr %1, align 4
        // store i32 1, ptr %2, align 4
        // %4 = getelementptr inbounds [2 x i32], ptr %3, i64 0, i64 0
        // %5 = load i32, ptr %2, align 4
        // store i32 %5, ptr %4, align 4
        // %6 = getelementptr inbounds i32, ptr %4, i64 1
        // store i32 2, ptr %6, align 4
        // ret i32 0
        //
        // this is:
        // int main() {
        // int x = 1;
        // int arr[2] = {x, 2};
        // return 0;
        let elem_ty = self.visit_expr(&mut exprs[0].kind).unwrap().ty.unwrap();
        let len = exprs.len() as u32;
        let array_type = elem_ty.array_type(len);
        let array_alloc = self.builder.build_alloca(array_type, "array").unwrap();
        //let array_val = self
        //    .builder
        //    .build_load(array_alloc, "array_load")
        //    .into_array_value();
        //
        //for (i, expr) in exprs.iter_mut().enumerate() {
        //    let v = self.traverse_expr(&mut expr.kind).unwrap();
        //    let mut val = v.value;
        //    if let BasicValueEnum::PointerValue(ptr) = val {
        //        val = self.builder.build_load( ptr, "load").unwrap();
        //    }
        //    self.builder
        //        .build_insert_value(array_val, val, i as u32, "insert");
        //}
        for (i, expr) in exprs.iter_mut().enumerate() {
            let e = self.visit_expr(&mut expr.kind).unwrap();
            let mut val = e.value;
            if let BasicValueEnum::PointerValue(ptr) = val {
                val = self.builder.build_load(ptr, "load").unwrap();
            }
            unsafe {
                let ptr = self
                    .builder
                    .build_in_bounds_gep(
                        array_alloc,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(i as u64, false),
                        ],
                        "ptr",
                    )
                    .unwrap();
                self.builder.build_store(ptr, val).unwrap();
            }
        }
        println!("ARRAY TYPE: {:?}", array_type);
        Some(CodeGenResult {
            value: array_alloc.into(),
            ty: Some(array_type.into()),
        })
    }

    fn visit_struct(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(StructInit, expr, ident, fields);
        let struct_ty = self.module.get_struct_type(ident).unwrap();
        let struct_val = self.builder.build_alloca(struct_ty, "struct").unwrap();

        for (i, field) in fields.iter_mut().enumerate() {
            let v = self.visit_expr(&mut field.kind).unwrap();
            let mut val = v.value;
            if let BasicValueEnum::PointerValue(ptr) = val {
                val = self.builder.build_load(ptr, "load").unwrap();
            }
            let field_ptr = self
                .builder
                .build_struct_gep(struct_val, i as u32, "field")
                .unwrap();
            self.builder.build_store(field_ptr, val);
        }

        Some(CodeGenResult {
            value: struct_val.into(),
            ty: Some(struct_ty.into()),
        })
    }

    fn visit_field(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(FieldAccess, expr, struct_, field);
        let struct_ = self.visit_expr(&mut struct_.kind).unwrap();
        let struct_val = struct_.value;
        let struct_ty = struct_.ty.unwrap();

        let struct_ptr = if let BasicValueEnum::PointerValue(ptr) = struct_val {
            ptr
        } else {
            self.builder.build_alloca(struct_ty, "struct").unwrap()
        };

        let ty = if let BasicTypeEnum::StructType(ty) = struct_ty {
            ty
        } else {
            struct_ty
                .into_pointer_type()
                .get_element_type()
                .into_struct_type()
        };

        let struct_name = ty.get_name().unwrap();
        let fields = self.structs.get(struct_name.to_str().unwrap()).unwrap();
        let field_index = fields.get(field).unwrap();
        let field_type = ty.get_field_type_at_index(*field_index as u32).unwrap();
        let field_ptr = self
            .builder
            .build_struct_gep(struct_ptr, *field_index as u32, "fieldaccess")
            .unwrap();
        Some(CodeGenResult {
            value: field_ptr.into(),
            ty: Some(field_type),
        })
    }

    fn visit_index(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(ArrayIndex, expr, array, index);
        let array = self.visit_expr(&mut array.kind).unwrap();
        let array_val = array.value;
        let array_ty = array.ty.unwrap();
        println!("array_ty: {:?}", array_ty);
        let array_ptr = if let BasicValueEnum::PointerValue(ptr) = array_val {
            ptr
        } else {
            self.builder.build_alloca(array_ty, "array").unwrap()
        };
        let ty = if let BasicTypeEnum::ArrayType(ty) = array_ty {
            ty
        } else {
            array_ty
                .into_pointer_type()
                .get_element_type()
                .into_array_type()
        };

        let elem_ty = ty.get_element_type();

        let index = self.visit_expr(&mut index.kind).unwrap();
        let index_val = if let BasicValueEnum::PointerValue(ptr) = index.value {
            self.builder
                .build_load(ptr, "load")
                .unwrap()
                .into_int_value()
        } else {
            index.value.into_int_value()
        };

        unsafe {
            let elem_ptr = self
                .builder
                .build_in_bounds_gep(
                    array_ptr,
                    &[self.context.i32_type().const_int(0, false), index_val],
                    "arrayaccess",
                )
                .unwrap();

            Some(CodeGenResult {
                value: elem_ptr.into(),
                ty: Some(elem_ty),
            })
        }
    }

    fn visit_call(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Call, expr, ident, params);
        let func = self.module.get_function(ident).unwrap();
        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
        let mut attributed_params: Vec<(u32, Attribute)> = Vec::new();
        for (i, param) in params.iter_mut().enumerate() {
            let (param_val, param_ty) = match *param.kind {
                ExprKind::FieldAccess(_, _) | ExprKind::ArrayIndex(_, _) => {
                    let res = self.visit_expr(&mut param.kind).unwrap();
                    let (val, ty) = (res.value, res.ty.unwrap());
                    let ptr = val.into_pointer_value();
                    (self.builder.build_load(ptr, "load").unwrap(), ty)
                }
                ExprKind::Var(_) => {
                    let res = self.visit_expr(&mut param.kind).unwrap();
                    let (val, ty) = (res.value, res.ty.unwrap());
                    match ty {
                        BasicTypeEnum::StructType(_) => (val, ty),
                        BasicTypeEnum::ArrayType(_) => (val, ty),
                        _ => {
                            let ptr = val.into_pointer_value();
                            (self.builder.build_load(ptr, "load").unwrap(), ty)
                        }
                    }
                }
                _ => {
                    let res = self.visit_expr(&mut param.kind).unwrap();
                    (res.value, res.ty.unwrap())
                }
            };
            let val = match param_ty {
                BasicTypeEnum::StructType(s) => {
                    println!("STRUCT");
                    if let ExprKind::Address(_) = *param.kind {
                        println!("ADDR");
                        param_val
                    } else {
                        println!("NOT ADDR");
                        let by_val = self.context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("byval"),
                            s.into(),
                        );
                        attributed_params.push((i as u32, by_val));
                        param_val
                    }
                }
                _ => param_val,
            };

            args.push(val.into());
        }

        let call = self.builder.build_call(func, &args, "call").unwrap();

        for (i, attr) in attributed_params.drain(..) {
            println!("attributing param {}", i);
            call.add_attribute(AttributeLoc::Param(i), attr);
        }

        let val = call.try_as_basic_value().left();
        //TODO: Fix this, void functions should return void
        let ret = val.unwrap_or_else(|| self.context.i32_type().const_zero().into());
        return Some(CodeGenResult {
            value: ret,
            ty: Some(ret.get_type()),
        });
    }

    fn visit_if(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(If, expr, cond, body);
        let cond = self.visit_expr(&mut cond.kind).unwrap();
        let cond_val = cond.value.into_int_value();
        let func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let then_block = self.context.append_basic_block(func, "then");
        let else_block = self.context.append_basic_block(func, "else");
        let end_block = self.context.append_basic_block(func, "end");
        self.builder
            .build_conditional_branch(cond_val, then_block, else_block);
        self.builder.position_at_end(then_block);
        self.traverse_block(body);
        self.builder.build_unconditional_branch(end_block);
        self.builder.position_at_end(else_block);
        self.builder.build_unconditional_branch(end_block);
        self.builder.position_at_end(end_block);
        None
    }

    fn visit_loop(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Loop, expr, cond, body);
        let start_block = self.builder.get_insert_block().unwrap();
        let func = start_block.get_parent().unwrap();
        let cond_block = self.context.append_basic_block(func, "loop_cond");
        let body_block = self.context.append_basic_block(func, "loop_body");
        let end_block = self.context.append_basic_block(func, "loop_end");
        self.current_loop_exit = Some(end_block);
        self.current_loop_cond = Some(cond_block);

        // Condition
        self.builder.build_unconditional_branch(cond_block);
        self.builder.position_at_end(cond_block);
        let cond = self.visit_expr(&mut cond.kind).unwrap();
        let cond_val = cond.value.into_int_value();

        // Loop
        self.builder
            .build_conditional_branch(cond_val, body_block, end_block);
        self.builder.position_at_end(body_block);
        self.traverse_block(body);

        // Exit
        self.builder.build_unconditional_branch(cond_block);
        self.builder.position_at_end(end_block);

        None
    }

    fn visit_assign(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Assign, expr, lhs_expr, rhs_expr);
        let rhs = self.visit_expr(&mut rhs_expr.kind).unwrap();
        let mut rhs_val = rhs.value;
        match *rhs_expr.kind {
            ExprKind::Var(_) | ExprKind::FieldAccess(_, _) | ExprKind::ArrayIndex(_, _) => {
                let ptr = rhs_val.into_pointer_value();
                rhs_val = self.builder.build_load(ptr, "load").unwrap();
            }
            _ => {}
        }

        let lhs = self.visit_expr(&mut lhs_expr.kind).unwrap();
        let lhs_ptr = lhs.value.into_pointer_value();
        self.builder.build_store(lhs_ptr, rhs_val);
        None
    }

    fn visit_var(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Var, expr, ident);
        let val = self.values.get(ident).unwrap();

        Some(CodeGenResult {
            value: val.0,
            ty: Some(val.1),
        })
    }

    fn visit_break(&mut self, _expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        let insert_block = self.builder.get_insert_block().unwrap();
        let end_block = self.current_loop_exit.unwrap();
        self.builder.build_unconditional_branch(end_block);
        let buffer_block = self
            .context
            .append_basic_block(insert_block.get_parent().unwrap(), "buffer");
        self.builder.position_at_end(buffer_block);
        None
    }

    fn visit_return(&mut self, expr: &mut ExprKind<'a>) -> Self::ExprReturnType {
        create_local_tuple!(Return, expr, expr);
        if let Some(expr) = expr {
            let expr = self.visit_expr(&mut expr.kind).unwrap();
            let mut expr_val = expr.value;
            if let BasicValueEnum::PointerValue(ptr) = expr_val {
                expr_val = self.builder.build_load(ptr, "loadret").unwrap();
            }
            self.builder.build_return(Some(&expr_val));
        } else {
            self.builder.build_return(None);
        }
        None
    }
}
