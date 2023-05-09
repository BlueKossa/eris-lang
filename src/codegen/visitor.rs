use std::{collections::HashMap, process::Command, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::BasicValueEnum, targets::{Target, InitializationConfig, TargetMachine, RelocMode, CodeModel, FileType}, OptimizationLevel,
};

use crate::{
    parser::ast::{
        blocks::Block,
        expressions::ExprKind,
        functions::FnDecl,
        items::{Item, ItemKind},
        locals::Local,
        operators::{BinaryOp, UnaryOp},
        statements::Statement,
        types::Type, literals::{Literal, LiteralKind},
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
        command.arg(path).arg("-o").arg("main");
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
        let value = match local.value {
            Some(ref mut expr) => self.traverse_expr(&mut expr.kind).unwrap().value,
            None => todo!(),
        };
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
                    LiteralKind::Int(i) => self.context.i64_type().const_int(i as u64, false).into(),
                    LiteralKind::Float(f) => self.context.f64_type().const_float(f).into(),
                    LiteralKind::String(_) => todo!(),
                    LiteralKind::Bool(b) => self.context.bool_type().const_int(b as u64, false).into(),
                };
                return Some(CodeGenResult {
                    value: val,
                    ty: Some(val.get_type())
                });
            }
            ExprKind::StructInit(_, _) => todo!(),
            ExprKind::FieldAccess(_, _) => todo!(),
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
                self.builder.build_store(lhs_val.into_pointer_value(), rhs_val);
                return None;
            }
            ExprKind::Call(_, _) => todo!(),
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
                        expr_val = self.builder.build_load(expr.ty.unwrap(), ptr, "load");
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
