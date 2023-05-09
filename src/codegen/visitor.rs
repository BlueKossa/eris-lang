use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, BasicMetadataValueEnum},
    OptimizationLevel,
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
        types::Type,
    },
    visitor::{chainmap::ChainMap, visitor_pattern::MutVisitorPattern},
};

pub struct CodeGenVisitor<'a> {
    builder: Builder<'a>,
    context: &'a Context,
    pub module: Module<'a>,
    values: ChainMap<&'a str, (BasicValueEnum<'a>, BasicTypeEnum<'a>)>,
    structs: HashMap<&'a str, HashMap<&'a str, usize>>,
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
        }
    }

    fn to_llvm_type(&mut self, ty: &Type<'a>) -> BasicTypeEnum<'a> {
        match ty {
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i8_type().into(),
            Type::Struct(ident) => {
                let struct_ty = self.module.get_struct_type(ident);
                if let Some(struct_ty) = struct_ty {
                    struct_ty.into()
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn to_llvm_fn_type(
        &mut self,
        ty: &Type<'a>,
        args: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        match ty {
            Type::I32 => self.context.i32_type().fn_type(args, false),
            Type::I64 => self.context.i64_type().fn_type(args, false),
            Type::F32 => self.context.f32_type().fn_type(args, false),
            Type::F64 => self.context.f64_type().fn_type(args, false),
            Type::Bool => self.context.bool_type().fn_type(args, false),
            Type::Char => self.context.i8_type().fn_type(args, false),
            Type::Void => self.context.void_type().fn_type(args, false),
            Type::Struct(ident) => {
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

    pub fn generate_object_file(&self, name: &str) {
        todo!();
    }

    pub fn declare_functions(
        &mut self,
        fn_decls: &HashMap<&'a str, Box<(Vec<Type<'a>>, Type<'a>)>>,
    ) {
        for (name, ptr) in fn_decls.iter() {
            let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            let (params, ret_ty) = ptr.as_ref();
            for param in params {
                param_types.push(self.to_llvm_type(param).into());
            }
            let fn_type = self.to_llvm_fn_type(ret_ty, &param_types);
            self.module.add_function(name, fn_type, None);
        }
    }

    pub fn declare_structs(&mut self, structs: &HashMap<&'a str, Box<Vec<(&'a str, Type<'a>)>>>) {
        for (name, ptr) in structs.iter() {
            let mut struct_types: HashMap<&'a str, usize> = HashMap::new();
            let mut types: Vec<BasicTypeEnum> = Vec::new();
            for (i, (n, ty)) in ptr.iter().enumerate() {
                types.push(self.to_llvm_type(ty));
                struct_types.insert(n, i);
            }
            let struct_type = self.context.opaque_struct_type(name);
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
        println!("CC:");
        let mut command = Command::new("cc");
        command.arg(path).arg("-o").arg("a.out");
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
        let ty = self.to_llvm_type(&local.ty.unwrap());
        let (mut value, lty) = match local.value {
            Some(ref mut expr) => {
                let res = self.traverse_expr(&mut expr.kind).unwrap();
                (res.value, res.ty.unwrap())
            },
            None => todo!(),
        };
        if let BasicValueEnum::PointerValue(ptr) = value {
            value = self.builder.build_load(lty, ptr, "load");
        }
        let alloca = self.builder.build_alloca(ty, local.ident);
        self.builder.build_store(alloca, value);
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
                    lhs_val = self.builder.build_load(lhs.ty.unwrap(), ptr, "load");
                }
                if let BasicValueEnum::PointerValue(ptr) = rhs_val {
                    rhs_val = self.builder.build_load(rhs.ty.unwrap(), ptr, "load");
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
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(ty),
                });
            }
            ExprKind::Unary(op, expr) => {
                let expr = self.traverse_expr(&mut expr.kind).unwrap();
                let mut expr_val = expr.value;
                if let BasicValueEnum::PointerValue(ptr) = expr_val {
                    expr_val = self.builder.build_load(expr.ty.unwrap(), ptr, "load");
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
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(ty),
                });
            }
            ExprKind::Literal(lit) => {
                let val: BasicValueEnum = match lit.kind {
                    LiteralKind::Int(i) => {
                        self.context.i32_type().const_int(i as u64, false).into()
                    }
                    LiteralKind::Float(f) => self.context.f64_type().const_float(f).into(),
                    LiteralKind::String(_) => todo!(),
                    LiteralKind::Bool(b) => {
                        self.context.bool_type().const_int(b as u64, false).into()
                    }
                };
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(val.get_type()),
                });
            }
            ExprKind::StructInit(ident, fields) => {
                let struct_ty = self.module.get_struct_type(ident).unwrap();
                let struct_val = self.builder.build_alloca(struct_ty, "struct");

                for (i, field) in fields.iter_mut().enumerate() {
                    let v = self.traverse_expr(&mut field.kind).unwrap();
                    let mut val = v.value;
                    if let BasicValueEnum::PointerValue(ptr) = val {
                        val = self.builder.build_load(
                            val.get_type().into_pointer_type(),
                            ptr,
                            "load",
                        );
                    }
                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_ty, struct_val, i as u32, "field")
                        .unwrap();
                    self.builder.build_store(field_ptr, val);
                }

                return Some(CodeGenResult {
                    value: struct_val.into(),
                    ty: Some(struct_ty.into()),
                });
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
                let ty = struct_ty.into_struct_type();
                let struct_name = ty.get_name().unwrap();
                let fields = self.structs.get(struct_name.to_str().unwrap()).unwrap();
                let field_index = fields.get(field).unwrap();
                let field_type = ty.get_field_type_at_index(*field_index as u32).unwrap();
                let field_ptr = self
                    .builder
                    .build_struct_gep(struct_ty.into_struct_type(), struct_ptr, *field_index as u32, "fieldaccess")
                    .unwrap();
                return Some(CodeGenResult {
                    value: field_ptr.into(),
                    ty: Some(field_type),
                });
            }
            ExprKind::MethodCall(_, _, _) => todo!(),
            ExprKind::If(_, _) => todo!(),
            ExprKind::While(_, _) => todo!(),
            ExprKind::Assign(lhs, rhs) => {
                let rhs = self.traverse_expr(&mut rhs.kind).unwrap();
                let mut rhs_val = rhs.value;
                if let BasicValueEnum::PointerValue(ptr) = rhs_val {
                    rhs_val = self.builder.build_load(rhs.ty.unwrap(), ptr, "load");
                }
                let lhs = self.traverse_expr(&mut lhs.kind).unwrap();
                let mut lhs_val = lhs.value;
                if let BasicValueEnum::PointerValue(ptr) = lhs_val {
                    lhs_val = self.builder.build_load(lhs.ty.unwrap(), ptr, "load");
                }
                self.builder
                    .build_store(lhs_val.into_pointer_value(), rhs_val);
                return None;
            }
            ExprKind::Call(ident, params) => {
                let func = self.module.get_function(ident).unwrap();
                    let mut args: Vec<BasicMetadataValueEnum> = Vec::new();
                for param in params {
                    let param = self.traverse_expr(&mut param.kind).unwrap();
                    let mut param_val = param.value;
                    if let BasicValueEnum::PointerValue(ptr) = param_val {
                        param_val = self.builder.build_load(param.ty.unwrap(), ptr, "load");
                    }
                    args.push(param_val.into());
                }

                let val = self
                    .builder
                    .build_call(func, &args, "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(val.get_type()),
                });


            }
            ExprKind::Var(ident) => {
                let val = self.values.get(ident).unwrap();
                return Some(CodeGenResult {
                    value: val.0,
                    ty: Some(val.1),
                });
            }
            ExprKind::Return(expr) => {
                if let Some(expr) = expr {
                    let expr = self.traverse_expr(&mut expr.kind).unwrap();
                    let mut expr_val = expr.value;
                    if let BasicValueEnum::PointerValue(ptr) = expr_val {
                        expr_val = self.builder.build_load(expr.ty.unwrap(), ptr, "loadret");
                    }
                    self.builder.build_return(Some(&expr_val));
                } else {
                    self.builder.build_return(None);
                }
                return None;
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
        if let None = func.get_type().get_return_type() {
            self.builder.build_return(None);
        }
        self.values.pop_map();

        None
    }
}
