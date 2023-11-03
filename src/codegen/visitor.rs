use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{
        AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType,
    },
    values::{
        AnyValue, AsValueRef, BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue,
    },
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
    visitor::{
        chainmap::ChainMap,
        visitor_pattern::{ExpressionVisitor, MutVisitorPattern},
    },
};

pub struct CodeGenVisitor<'a> {
    pub(super) builder: Builder<'a>,
    pub(super) context: &'a Context,
    pub(super) module: Module<'a>,
    pub(super) values: ChainMap<&'a str, (BasicValueEnum<'a>, BasicTypeEnum<'a>)>,
    pub(super) structs: HashMap<&'a str, HashMap<&'a str, usize>>,

    pub(super) current_loop_exit: Option<BasicBlock<'a>>,
    pub(super) current_loop_cond: Option<BasicBlock<'a>>,
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

    pub(super) fn to_llvm_type(&self, ty: &Type<'a>) -> BasicTypeEnum<'a> {
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
                // NULL TERMINATED = +1 len
                let arr_ty = ty.array_type(*len as u32 + 1);
                dbg!(arr_ty.as_basic_type_enum());
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
        match &*ty.kind {
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
            TypeKind::Array(ty, len) => {
                let ty = self.to_llvm_type(ty);
                let arr_ty = ty.array_type(*len as u32);
                arr_ty.fn_type(args, false)
            }
            _ => todo!(),
        }
    }

    pub fn declare_functions(&mut self, fn_decls: &HashMap<&'a str, (Vec<Type<'a>>, Type<'a>)>) {
        use BasicTypeEnum as BTE;
        for (name, sig) in fn_decls.iter() {
            let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            let mut attributed_params = Vec::new();
            let (params, ret_ty) = sig;
            for (i, param) in params.iter().enumerate() {
                let ty = match self.to_llvm_type(param) {
                    BTE::StructType(s) => {
                        let by_val = self.context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("byval"),
                            s.into(),
                        );
                        attributed_params.push((i as u32, by_val));
                        let ptr_ty = s.ptr_type(AddressSpace::default());
                        ptr_ty.into()
                    }
                    BTE::ArrayType(a) => {
                        let by_val = self.context.create_type_attribute(
                            Attribute::get_named_enum_kind_id("byval"),
                            a.into(),
                        );
                        attributed_params.push((i as u32, by_val));
                        let ptr_ty = a.ptr_type(AddressSpace::default());
                        ptr_ty.into()
                    }
                    _ => {
                        let ty = self.to_llvm_type(param);
                        ty
                    }
                };
                param_types.push(ty.into());
            }
            let fn_type = self.to_llvm_fn_type(ret_ty, &param_types);

            let func = self.module.add_function(name, fn_type, None);
            for (i, attr) in attributed_params.drain(..) {
                func.add_attribute(AttributeLoc::Param(i), attr);
            }
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
    pub value: BasicValueEnum<'a>,
    pub ty: Option<BasicTypeEnum<'a>>,
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
                self.visit_expr(&mut expr.kind);
                None
            }
        }
    }

    fn traverse_local(&mut self, local: &mut Local<'a>) -> Self::ReturnType {
        let ty = self.to_llvm_type(local.ty.as_ref().unwrap());
        println!("ty: {:?}", ty);
        println!("local: {:?}", local.ty);
        let (value, lty) = match local.value {
            Some(ref mut expr) => {
                let res = self.visit_expr(&mut expr.kind).unwrap();
                let (mut value, lty) = (res.value, res.ty.unwrap());
                println!("val: {}, ty: {}", local.ident, lty);
                match *expr.kind {
                    ExprKind::Var(_) | ExprKind::FieldAccess(_, _) | ExprKind::ArrayIndex(_, _) => {
                        let ptr = value.into_pointer_value();
                        value = self.builder.build_load(ptr, "load").unwrap();
                    }
                    _ => {}
                }
                if ty != lty {
                    let value = value.into_int_value().const_cast(ty.into_int_type(), false);
                    (value.into(), ty)
                } else {
                    (value, lty)
                }
            }
            None => {
                //REVIEW: Can you do this?
                let alloca = self.builder.build_alloca(ty, "").unwrap();
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
            let alloca = self.builder.build_alloca(ty, "").unwrap();

            ptr.replace_all_uses_with(alloca);
            ptr.as_instruction_value()
                .unwrap()
                .remove_from_basic_block();

            self.builder.position_at_end(current_block);

            alloca
        } else {
            let alloca = self.builder.build_alloca(ty, "").unwrap();
            self.builder.position_at_end(current_block);
            self.builder.build_store(alloca, value).unwrap();
            alloca
        };

        self.values.insert(local.ident, (alloca.into(), ty));
        None
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
            dbg!(self.to_llvm_type(&arg.1));
            self.values.insert(arg.0, (a, self.to_llvm_type(&arg.1)));
        }
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.traverse_block(&mut function.body);
        if func.get_type().get_return_type().is_none() {
            self.builder.build_return(None).unwrap();
        }
        self.values.pop_map();

        None
    }
}
