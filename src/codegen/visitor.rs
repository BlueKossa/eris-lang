use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{
        AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
    },
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    parser::ast::{
        blocks::Block,
        expressions::ExprKind,
        functions::FnDecl,
        items::{Item, ItemKind},
        literals::{Literal, LiteralKind},
        locals::Local,
        operators::{BinaryOp, UnaryOp},
        statements::Statement,
        types::{Type, TypeKind},
    },
    visitor::{chainmap::ChainMap, visitor_pattern::MutVisitorPattern},
};

pub struct CodeGenVisitor<'a> {
    builder: Builder<'a>,
    context: &'a Context,
    pub module: Module<'a>,
    values: ChainMap<&'a str, (BasicValueEnum<'a>, BasicTypeEnum<'a>)>,
    structs: HashMap<&'a str, HashMap<&'a str, usize>>,

    current_loop_exit: Option<BasicBlock<'a>>,
    current_loop_cond: Option<BasicBlock<'a>>,
}

impl<'a> CodeGenVisitor<'a> {
    pub fn new(context: &'a Context, name: &'a str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let values = ChainMap::new();
        let structs = HashMap::new();
        Self {
            builder,
            context,
            module,
            values,
            structs,

            current_loop_exit: None,
            current_loop_cond: None,
        }
    }

    fn to_llvm_type(&self, ty: &Type<'a>) -> BasicTypeEnum<'a> {
        match &*ty.kind {
            TypeKind::I8 => self.context.i8_type().into(),
            TypeKind::I16 => self.context.i16_type().into(),
            TypeKind::I32 => self.context.i32_type().into(),
            TypeKind::I64 => self.context.i64_type().into(),
            TypeKind::U8 => self.context.i8_type().into(),
            TypeKind::U16 => self.context.i16_type().into(),
            TypeKind::U32 => self.context.i32_type().into(),
            TypeKind::U64 => self.context.i64_type().into(),
            TypeKind::F32 => self.context.f32_type().into(),
            TypeKind::F64 => self.context.f64_type().into(),
            TypeKind::Bool => self.context.bool_type().into(),
            TypeKind::Char => self.context.i8_type().into(),
            TypeKind::Struct(ident) => {
                let struct_ty = self.module.get_struct_type(ident);
                if let Some(struct_ty) = struct_ty {
                    struct_ty.into()
                } else {
                    todo!()
                }
            }
            TypeKind::Str => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .into(),
            TypeKind::Array(ty, len) => {
                let ty = self.to_llvm_type(ty);
                let arr_ty = ty.array_type(*len as u32);
                arr_ty.into()
            }
            // TODO: Fix this, void is not supposed to be an int
            TypeKind::Void => self.context.i8_type().into(),
            TypeKind::Ref(ty) => {
                let ty = self.to_llvm_type(ty);
                ty.ptr_type(AddressSpace::default()).into()
            }
            t => todo!("{:?}", t),
        }
    }

    fn to_llvm_fn_type(
        &self,
        ty: &Type<'a>,
        args: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        match *ty.kind {
            TypeKind::I32 => self.context.i32_type().fn_type(args, false),
            TypeKind::I64 => self.context.i64_type().fn_type(args, false),
            TypeKind::F32 => self.context.f32_type().fn_type(args, false),
            TypeKind::F64 => self.context.f64_type().fn_type(args, false),
            TypeKind::Bool => self.context.bool_type().fn_type(args, false),
            TypeKind::Char => self.context.i8_type().fn_type(args, false),
            TypeKind::Void => self.context.void_type().fn_type(args, false),
            TypeKind::Struct(ident) => {
                let struct_ty = self.module.get_struct_type(ident);
                if let Some(struct_ty) = struct_ty {
                    struct_ty.fn_type(args, false)
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }


    pub fn declare_functions(&mut self, fn_decls: &HashMap<&'a str, (Vec<Type<'a>>, Type<'a>)>) {
        for (name, sig) in fn_decls.iter() {
            let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            let (params, ret_ty) = sig;
            for param in params {
                param_types.push(self.to_llvm_type(param).into());
            }
            let fn_type = self.to_llvm_fn_type(ret_ty, &param_types);
            self.module.add_function(name, fn_type, None);
        }
    }

    pub fn create_clib_functions(&mut self) {
        let i32_type = self.context.i32_type();
        let char_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let fn_ty = i32_type.fn_type(&[char_type.into()], false);
        self.module.add_function("printf", fn_ty, None);
        let fn_ty = i32_type.fn_type(&[i32_type.into()], false);
        self.module.add_function("putchar", fn_ty, None);
    }

    pub fn declare_structs(&mut self, structs: &HashMap<&'a str, Vec<(&'a str, Type<'a>)>>) {
        // Add all structs to the module
        for (name, _) in structs.iter() {
            self.context.opaque_struct_type(name);
        }
        // Generate the structs
        for (name, ptr) in structs.iter() {
            let mut struct_types: HashMap<&'a str, usize> = HashMap::new();
            let mut types: Vec<BasicTypeEnum> = Vec::new();
            for (i, (n, ty)) in ptr.iter().enumerate() {
                types.push(self.to_llvm_type(ty));
                struct_types.insert(n, i);
            }
            let struct_type = self.context.get_struct_type(name).unwrap();
            struct_type.set_body(&types, false);

            self.structs.insert(name, struct_types);
        }
    }

    pub fn run(&mut self, block: &mut Block<'a>) {
        self.traverse_block(block);
    }

    pub fn dump(&self) {
        self.module.print_to_stderr();
    }

    pub fn dump_file(&self, path: &str) {
        self.module.print_to_file(path).unwrap();
    }

    #[cfg(target_os = "linux")]
    pub fn generate_machine_code(&self, path: &str) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let reloc_model = RelocMode::PIC;
        let code_model = CodeModel::Default;
        let opt_level = OptimizationLevel::Aggressive;
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                opt_level,
                reloc_model,
                code_model,
            )
            .unwrap();
        let file_type = FileType::Object;

        target_machine
            .write_to_file(&self.module, file_type, Path::new(path))
            .unwrap();

        let mut command = Command::new("cc");
        command.arg(path).arg("-o").arg("a.out");
        let _r = command.output().unwrap();
    }

    #[cfg(target_os = "windows")]
    pub fn generate_machine_code(&self, path: &str) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let reloc_model = RelocMode::PIC;
        let code_model = CodeModel::Default;
        let opt_level = OptimizationLevel::Aggressive;
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                opt_level,
                reloc_model,
                code_model,
            )
            .unwrap();
        let file_type = FileType::Object;
        target_machine
            .write_to_file(&self.module, file_type, Path::new(path))
            .unwrap();

        let mut command = Command::new("gcc");
        command.arg(path).arg("-o").arg("a.exe");
        let r = command.output().unwrap();
    }
}

pub struct CodeGenResult<'a> {
    value: BasicValueEnum<'a>,
    ty: Option<BasicTypeEnum<'a>>,
}

impl<'a> MutVisitorPattern<'a> for CodeGenVisitor<'a> {
    type ReturnType = Option<CodeGenResult<'a>>;

    fn traverse_block(&mut self, block: &mut Block<'a>) -> Self::ReturnType {
        self.values.insert_map();
        for statement in block.statements.iter_mut() {
            self.traverse_statement(statement);
        }
        self.values.pop_map();
        None
    }

    fn traverse_statement(&mut self, statement: &mut Statement<'a>) -> Self::ReturnType {
        match statement {
            Statement::Local(local) => {
                self.traverse_local(local);
                None
            }
            Statement::Item(item) => {
                self.traverse_item(item);
                None
            }
            Statement::Expression(expr) => {
                self.traverse_expr(&mut expr.kind);
                None
            }
        }
    }

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ReturnType {
        println!("Local: {}", local.ident);
        let ty = self.to_llvm_type(local.ty.as_ref().unwrap());
        let (value, lty) = match local.value {
            Some(ref mut expr) => {
                let res = self.traverse_expr(&mut expr.kind).unwrap();
                let (mut value, lty) = (res.value, res.ty.unwrap());
                match *expr.kind {
                    ExprKind::Var(_)
                    | ExprKind::FieldAccess(_, _)
                    | ExprKind::ArrayAccess(_, _) => {
                        value = self.builder.build_load(value.into_pointer_value(), "load");
                    }
                    _ => {}
                }
                if ty != lty {
                    println!("{:?} {:?}", ty, lty);
                    let value = value.into_int_value().const_cast(ty.into_int_type(), false);
                    (value.into(), ty)
                } else {
                    (value, lty)
                }
            }
            None => {
                //REVIEW: Can you do this?
                let alloca = self.builder.build_alloca(ty, "");
                self.values.insert(local.ident, (alloca.into(), ty));
                return None;
            }
        };
        let current_block = self.builder.get_insert_block().unwrap();

        let entry_block = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
            .get_first_basic_block()
            .unwrap();

        let first_instruction = entry_block.get_first_instruction();

        if let Some(first_instruction) = first_instruction {
            self.builder.position_before(&first_instruction);
        } else {
            self.builder.position_at_end(entry_block);
        }

        let alloca = if let BasicValueEnum::PointerValue(ptr) = value {
            if !lty.is_pointer_type() {
                //value = self.builder.build_load(ptr, "load");
            }
            let alloca = self.builder.build_alloca(ty, "");

            ptr.replace_all_uses_with(alloca);
            ptr.as_instruction_value()
                .unwrap()
                .remove_from_basic_block();

            self.builder.position_at_end(current_block);

            alloca
        } else {
            let alloca = self.builder.build_alloca(ty, "");
            self.builder.position_at_end(current_block);
            self.builder.build_store(alloca, value);
            alloca
        };

        self.values.insert(local.ident, (alloca.into(), ty));
        None
    }

    fn traverse_expr(&mut self, expr: &mut ExprKind<'a>) -> Self::ReturnType {
        match expr {
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.traverse_expr(&mut lhs.kind).unwrap();
                let rhs = self.traverse_expr(&mut rhs.kind).unwrap();
                let mut lhs_val = lhs.value;
                let mut rhs_val = rhs.value;
                if let BasicValueEnum::PointerValue(ptr) = lhs_val {
                    lhs_val = self.builder.build_load(ptr, "load");
                }
                if let BasicValueEnum::PointerValue(ptr) = rhs_val {
                    rhs_val = self.builder.build_load(ptr, "load");
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
                            BinaryOp::Equal => self.builder.build_int_compare(
                                inkwell::IntPredicate::EQ,
                                a,
                                b,
                                "eq",
                            ),
                            BinaryOp::NotEqual => self.builder.build_int_compare(
                                inkwell::IntPredicate::NE,
                                a,
                                b,
                                "ne",
                            ),
                            BinaryOp::LessThan => self.builder.build_int_compare(
                                inkwell::IntPredicate::SLT,
                                a,
                                b,
                                "lt",
                            ),
                            BinaryOp::LessThanEqual => self.builder.build_int_compare(
                                inkwell::IntPredicate::SLE,
                                a,
                                b,
                                "le",
                            ),
                            BinaryOp::GreaterThan => self.builder.build_int_compare(
                                inkwell::IntPredicate::SGT,
                                a,
                                b,
                                "gt",
                            ),
                            BinaryOp::GreaterThanEqual => self.builder.build_int_compare(
                                inkwell::IntPredicate::SGE,
                                a,
                                b,
                                "ge",
                            ),
                            o => unimplemented!("{:?}", o),
                        };
                        res.into()
                    }
                    (BasicValueEnum::FloatValue(a), BasicValueEnum::FloatValue(b)) => {
                        let res: BasicValueEnum = match op {
                            BinaryOp::Add => self.builder.build_float_add(a, b, "add").into(),
                            BinaryOp::Subtract => self.builder.build_float_sub(a, b, "sub").into(),
                            BinaryOp::Multiply => self.builder.build_float_mul(a, b, "mul").into(),
                            BinaryOp::Divide => self.builder.build_float_div(a, b, "div").into(),
                            BinaryOp::Modulo => self.builder.build_float_rem(a, b, "rem").into(),
                            BinaryOp::Equal => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OEQ, a, b, "eq")
                                .into(),
                            BinaryOp::NotEqual => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::ONE, a, b, "ne")
                                .into(),
                            BinaryOp::LessThan => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OLT, a, b, "lt")
                                .into(),
                            BinaryOp::LessThanEqual => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OLE, a, b, "le")
                                .into(),
                            BinaryOp::GreaterThan => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OGT, a, b, "gt")
                                .into(),
                            BinaryOp::GreaterThanEqual => self
                                .builder
                                .build_float_compare(inkwell::FloatPredicate::OGE, a, b, "ge")
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
            ExprKind::Unary(op, expr) => {
                let expr = self.traverse_expr(&mut expr.kind).unwrap();
                let mut expr_val = expr.value;
                if let BasicValueEnum::PointerValue(ptr) = expr_val {
                    expr_val = self.builder.build_load(ptr, "load");
                }
                let ty = expr.ty.unwrap();
                let val: BasicValueEnum = match expr_val {
                    BasicValueEnum::IntValue(a) => {
                        let res = match op {
                            UnaryOp::Negate => self.builder.build_int_neg(a, "neg"),
                            UnaryOp::Not => self.builder.build_not(a, "not"),
                        };
                        res.into()
                    }
                    BasicValueEnum::FloatValue(a) => {
                        let res = match op {
                            UnaryOp::Negate => self.builder.build_float_neg(a, "neg"),
                            o => unimplemented!("{:?}", o),
                        };
                        res.into()
                    }
                    _ => unimplemented!(),
                };
                Some(CodeGenResult {
                    value: val,
                    ty: Some(ty),
                })
            }
            ExprKind::Literal(lit) => {
                let val: BasicValueEnum = match lit.kind {
                    LiteralKind::Int(i) => {
                        self.context.i32_type().const_int(i as u64, false).into()
                    }
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
                        let string = self.builder.build_global_string_ptr(&s, "string");
                        let ptr = string.as_pointer_value();
                        return Some(CodeGenResult {
                            value: ptr.into(),
                            ty: Some(ptr.get_type().into()),
                        });
                    }
                    LiteralKind::Bool(b) => {
                        self.context.bool_type().const_int(b as u64, false).into()
                    }
                };
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(val.get_type()),
                });
            }
            ExprKind::Ref(expr) => {
                if let ExprKind::Var(_var) = *expr.kind {
                    return self.traverse_expr(&mut expr.kind);
                }
                let expr = self.traverse_expr(&mut expr.kind).unwrap();
                let ty = expr.ty.unwrap();
                let val = expr.value;
                let ptr = val.into_pointer_value();
                Some(CodeGenResult {
                    value: ptr.into(),
                    ty: Some(ty),
                })
            }

            ExprKind::Array(exprs) => {
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
                let elem_ty = self.traverse_expr(&mut exprs[0].kind).unwrap().ty.unwrap();
                let len = exprs.len() as u32;
                let array_type = elem_ty.array_type(len);
                let array_alloc = self.builder.build_alloca(array_type, "array");
                //let array_val = self
                //    .builder
                //    .build_load(array_alloc, "array_load")
                //    .into_array_value();
                //
                //for (i, expr) in exprs.iter_mut().enumerate() {
                //    let v = self.traverse_expr(&mut expr.kind).unwrap();
                //    let mut val = v.value;
                //    if let BasicValueEnum::PointerValue(ptr) = val {
                //        val = self.builder.build_load(ptr, "load");
                //    }
                //    self.builder
                //        .build_insert_value(array_val, val, i as u32, "insert");
                //}
                for (i, expr) in exprs.iter_mut().enumerate() {
                    let e = self.traverse_expr(&mut expr.kind).unwrap();
                    let mut val = e.value;
                    if let BasicValueEnum::PointerValue(ptr) = val {
                        val = self.builder.build_load(ptr, "load");
                    }
                    unsafe {
                        let ptr = self.builder.build_in_bounds_gep(
                            array_alloc,
                            &[
                                self.context.i32_type().const_int(0, false),
                                self.context.i32_type().const_int(i as u64, false),
                            ],
                            "ptr",
                        );
                        self.builder.build_store(ptr, val);
                    }
                }
                Some(CodeGenResult {
                    value: array_alloc.into(),
                    ty: Some(array_type.into()),
                })
            }
            ExprKind::StructInit(ident, fields) => {
                let struct_ty = self.module.get_struct_type(ident).unwrap();
                let struct_val = self.builder.build_alloca(struct_ty, "struct");

                for (i, field) in fields.iter_mut().enumerate() {
                    let v = self.traverse_expr(&mut field.kind).unwrap();
                    let mut val = v.value;
                    if let BasicValueEnum::PointerValue(ptr) = val {
                        val = self.builder.build_load(ptr, "load");
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
            ExprKind::FieldAccess(struct_, field) => {
                let struct_ = self.traverse_expr(&mut struct_.kind).unwrap();
                let struct_val = struct_.value;
                let struct_ty = struct_.ty.unwrap();

                let struct_ptr = if let BasicValueEnum::PointerValue(ptr) = struct_val {
                    ptr
                } else {
                    self.builder.build_alloca(struct_ty, "struct")
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
                println!("field_ptr: {:?}", field_ptr);
                println!("field_type: {:?}", field_type);
                Some(CodeGenResult {
                    value: field_ptr.into(),
                    ty: Some(field_type),
                })
            }
            ExprKind::ArrayAccess(array, index) => {
                let array = self.traverse_expr(&mut array.kind).unwrap();
                let array_val = array.value;
                let array_ty = array.ty.unwrap();
                let array_ptr = if let BasicValueEnum::PointerValue(ptr) = array_val {
                    ptr
                } else {
                    self.builder.build_alloca(array_ty, "array")
                };
                let ty = array_ty.into_array_type();
                let elem_ty = ty.get_element_type();

                let index = self.traverse_expr(&mut index.kind).unwrap();
                let index_val = if let BasicValueEnum::PointerValue(ptr) = index.value {
                    self.builder.build_load(ptr, "load").into_int_value()
                } else {
                    index.value.into_int_value()
                };

                unsafe {
                    let elem_ptr = self.builder.build_in_bounds_gep(
                        array_ptr,
                        &[self.context.i32_type().const_int(0, false), index_val],
                        "arrayaccess",
                    );

                    Some(CodeGenResult {
                        value: elem_ptr.into(),
                        ty: Some(elem_ty),
                    })
                }
            }
            ExprKind::MethodCall(_, _, _) => todo!(),
            ExprKind::If(cond, body) => {
                let cond = self.traverse_expr(&mut cond.kind).unwrap();
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
            ExprKind::Loop(cond, body) => {
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
                let cond = self.traverse_expr(&mut cond.kind).unwrap();
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
            ExprKind::Assign(lhs, rhs_expr) => {
                let rhs = self.traverse_expr(&mut rhs_expr.kind).unwrap();
                let mut rhs_val = rhs.value;
                match *rhs_expr.kind {
                    ExprKind::Var(_)
                    | ExprKind::FieldAccess(_, _)
                    | ExprKind::ArrayAccess(_, _) => {
                        rhs_val = self
                            .builder
                            .build_load(rhs_val.into_pointer_value(), "load");
                    }
                    _ => {}
                }

                let lhs = self.traverse_expr(&mut lhs.kind).unwrap();
                let lhs_ptr = lhs.value.into_pointer_value();
                self.builder.build_store(lhs_ptr, rhs_val);
                None
            }
            ExprKind::Call(ident, params) => {
                let func = self.module.get_function(ident).unwrap();
                let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                for param in params {
                    let param_val = match *param.kind {
                        ExprKind::Var(_)
                        | ExprKind::FieldAccess(_, _)
                        | ExprKind::ArrayAccess(_, _) => {
                            let res = self.traverse_expr(&mut param.kind).unwrap();
                            let (val, _ty) = (res.value, res.ty.unwrap());
                            self.builder.build_load(val.into_pointer_value(), "load")
                        }
                        _ => self.traverse_expr(&mut param.kind).unwrap().value,
                    };
                    args.push(param_val.into());
                }

                let val = self
                    .builder
                    .build_call(func, &args, "call")
                    .try_as_basic_value()
                    .left();
                //TODO: Fix this, void functions should return void
                let ret = val.unwrap_or_else(|| self.context.i32_type().const_zero().into());
                return Some(CodeGenResult {
                    value: ret,
                    ty: Some(ret.get_type()),
                });
            }
            ExprKind::Var(ident) => {
                let val = self.values.get(ident).unwrap();
                Some(CodeGenResult {
                    value: val.0,
                    ty: Some(val.1),
                })
            }
            ExprKind::Break => {
                let insert_block = self.builder.get_insert_block().unwrap();
                let end_block = self.current_loop_exit.unwrap();
                self.builder.build_unconditional_branch(end_block);
                let buffer_block = self
                    .context
                    .append_basic_block(insert_block.get_parent().unwrap(), "buffer");
                self.builder.position_at_end(buffer_block);
                None
            }
            ExprKind::Return(expr) => {
                if let Some(expr) = expr {
                    let expr = self.traverse_expr(&mut expr.kind).unwrap();
                    let mut expr_val = expr.value;
                    if let BasicValueEnum::PointerValue(ptr) = expr_val {
                        expr_val = self.builder.build_load(ptr, "loadret");
                    }
                    self.builder.build_return(Some(&expr_val));
                } else {
                    self.builder.build_return(None);
                }
                None
            }
        }
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> Self::ReturnType {
        match &mut item.kind {
            ItemKind::Function(func) => {
                self.traverse_function(func);
            }
            ItemKind::Struct(_) => {}
            ItemKind::Constant(_, _, _) => todo!(),
        }
        None
    }

    fn traverse_function(&mut self, function: &mut FnDecl<'a>) -> Self::ReturnType {
        let sig = &function.sig;
        let func = self.module.get_function(function.name).unwrap();
        self.values.insert_map();
        for (i, arg) in sig.args.iter().enumerate() {
            let a = func.get_nth_param(i as u32).unwrap();
            self.values.insert(arg.0, (a, a.get_type()));
        }
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.traverse_block(&mut function.body);
        if func.get_type().get_return_type().is_none() {
            self.builder.build_return(None);
        }
        self.values.pop_map();

        None
    }
}
