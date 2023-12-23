use std::{collections::HashMap, path::Path, process::Command, rc::Rc};

use inkwell::{
    attributes::{Attribute, AttributeLoc},
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module as LLVMModule,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{
        BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, MetadataType,
    },
    values::{
        BasicValue, BasicValueEnum,
    },
    AddressSpace, OptimizationLevel,
};

use crate::{
    parser::ast::{
        blocks::Block,
        expressions::ExprKind,
        functions::{FnDecl, FnSig},
        items::{Item, ItemKind},
        locals::Local,
        statements::Statement,
        types::{Type, TypeKind},
    },
    visitor::{
        chainmap::ChainMap,
        visitor_pattern::{ExpressionVisitor, MutVisitorPattern},
    }, module::{ModuleMap, Module}, span::Span, semantic::semantic_visitor::SemanticVisitor,
};

pub struct CodeGenVisitor<'a> {
    pub(super) builder: Builder<'a>,
    pub(super) context: &'a Context,
    pub(super) modules: ModuleMap<'a>,
    pub(super) module_idx: Option<usize>,
    pub(super) values: ChainMap<Rc<str>, (BasicValueEnum<'a>, BasicTypeEnum<'a>)>,
    pub(super) structs: HashMap<Rc<str>, HashMap<Rc<str>, usize>>,

    pub(super) current_loop_exit: Option<BasicBlock<'a>>,
    pub(super) current_loop_cond: Option<BasicBlock<'a>>,
}

impl<'a> CodeGenVisitor<'a> {
    pub fn new(context: &'a Context) -> Self {
        let modules = ModuleMap::new();
        let builder = context.create_builder();
        let values = ChainMap::new();
        let structs = HashMap::new();
        Self {
            builder,
            context,
            modules,
            module_idx: None,
            values,
            structs,

            current_loop_exit: None,
            current_loop_cond: None,
        }
    }

    pub fn get_data(&self, span: &Span) -> Option<Rc<str>> {
        if let Some(idx) = self.module_idx {
            self.modules.get_module(idx).map(|m| {
                m.get_data(span)
            })
        } else {
            None
        }
    }

    pub fn get_current_module(&self) -> Option<&Module<'a>> {
        if let Some(idx) = self.module_idx {
            self.modules.get_module(idx).map(|m| m)
        } else {
            None
        }
    }

    pub fn get_current_llvm_module(&self) -> Option<&LLVMModule<'a>> {
        self.get_current_module().map(|m| &m.llvm_module)
    }

    pub(super) fn to_llvm_type(&self, ty: &Type) -> BasicTypeEnum<'a> {
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
                let module = self.get_current_llvm_module().unwrap();
                let struct_ty = module.get_struct_type(ident);
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
        ty: &Type,
        args: &[BasicMetadataTypeEnum<'a>],
        variadic: bool,
    ) -> FunctionType<'a> {
        match &*ty.kind {
            TypeKind::I32 => self.context.i32_type().fn_type(args, variadic),
            TypeKind::I64 => self.context.i64_type().fn_type(args, variadic),
            TypeKind::F32 => self.context.f32_type().fn_type(args, variadic),
            TypeKind::F64 => self.context.f64_type().fn_type(args, variadic),
            TypeKind::Bool => self.context.bool_type().fn_type(args, variadic),
            TypeKind::Char => self.context.i8_type().fn_type(args, variadic),
            TypeKind::Void => self.context.void_type().fn_type(args, variadic),
            TypeKind::Struct(ident) => {
                let module = self.get_current_llvm_module().unwrap();
                let struct_ty = module.get_struct_type(ident);
                if let Some(struct_ty) = struct_ty {
                    struct_ty.fn_type(args, variadic)
                } else {
                    todo!()
                }
            }
            TypeKind::Array(ty, len) => {
                let ty = self.to_llvm_type(ty);
                // NULL TERMINATED = +1 len
                let arr_ty = ty.array_type(*len as u32 + 1);
                arr_ty.fn_type(args, variadic)
            }
            TypeKind::Ref(ty) => {
                let ty = self.to_llvm_type(ty);
                ty.ptr_type(AddressSpace::default()).fn_type(args, variadic)
            }
            _ => todo!(),
        }
    }

    pub fn declare_functions(&mut self, fn_decls: &HashMap<Rc<str>, FnSig>) {
        use BasicTypeEnum as BTE;
        for (name, sig) in fn_decls {
            let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
            let mut attributed_params = Vec::new();
            for (i, (_name, ty)) in sig.args.iter().enumerate() {
                let ty = match self.to_llvm_type(ty) {
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
                        let ty = self.to_llvm_type(ty);
                        ty
                    }
                };
                param_types.push(ty.into());
            }
            let fn_type = self.to_llvm_fn_type(&sig.ret, &param_types, sig.is_variadic);
            let module = self.get_current_llvm_module().unwrap();
            let func = module.add_function(name, fn_type, None);
            for (i, attr) in attributed_params.drain(..) {
                func.add_attribute(AttributeLoc::Param(i), attr);
            }
        }
    }

    pub fn declare_structs(&mut self, structs: &HashMap<Rc<str>, Vec<(Rc<str>, Type)>>) {
        // Add all structs to the module
        for (name, _) in structs.iter() {
            self.context.opaque_struct_type(name);
        }
        // Generate the structs
        for (name, ptr) in structs.iter() {
            let mut struct_types: HashMap<Rc<str>, usize> = HashMap::new();
            let mut types: Vec<BasicTypeEnum> = Vec::new();
            for (i, (n, ty)) in ptr.iter().enumerate() {
                types.push(self.to_llvm_type(ty));
                struct_types.insert(n.to_owned(), i);
            }
            let struct_type = self.context.get_struct_type(name).unwrap();
            struct_type.set_body(&types, false);

            self.structs.insert(name.to_owned(), struct_types);
        }
    }

    pub fn run(&mut self, root: &str) {
        let root_module = self.context.create_module(root);
        let idx = self.modules.add_module(root, root_module);
        self.module_idx = Some(idx);
        let module = self.get_current_module().unwrap();
        let mut ast = module.ast.clone();
        let mut semantic_visitor = SemanticVisitor::new();
        semantic_visitor.run(&mut ast, module.source.clone());
        self.declare_structs(&semantic_visitor.structs);
        self.declare_functions(&semantic_visitor.fn_decls);

        self.traverse_block(&mut ast);
        self.dump_file();
        self.generate_machine_code("a.out");
    }

    pub fn dump(&self) {
        for module in &self.modules.modules {
            module.llvm_module.print_to_stderr();
        }
    }

    pub fn dump_file(&self) {
        for module in &self.modules.modules {
            let path = format!("./out/{}.ll", module.name);
            module.llvm_module.print_to_file(path).unwrap();
        }
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

        let object_file_name = format!("{}.o", path);
        let module = self.get_current_llvm_module().unwrap();
        module.print_to_stderr();

        target_machine
            .write_to_file(&module, file_type, Path::new(object_file_name.as_str()))
            .unwrap();

        let mut command = Command::new("cc");
        command.arg(object_file_name).arg("-o").arg(path);
        let r = command.output().unwrap();
        println!("{}", String::from_utf8_lossy(&r.stdout));
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

    fn traverse_block(&mut self, block: &mut Block) -> Self::ReturnType {
        self.values.insert_map();
        for statement in block.statements.iter_mut() {
            self.traverse_statement(statement);
        }
        self.values.pop_map();
        None
    }

    fn traverse_statement(&mut self, statement: &mut Statement) -> Self::ReturnType {
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

    fn traverse_local(&mut self, local: &mut Local) -> Self::ReturnType {
        let ty = self.to_llvm_type(local.ty.as_ref().unwrap());
        let (value, lty) = match local.value {
            Some(ref mut expr) => {
                let res = self.visit_expr(&mut expr.kind).unwrap();
                let (mut value, lty) = (res.value, res.ty.unwrap());
                match *expr.kind {
                    ExprKind::Var(_) | ExprKind::FieldAccess(_, _) | ExprKind::ArrayIndex(_, _) => {
                        let ptr = value.into_pointer_value();
                        value = self.builder.build_load(ptr, "load").unwrap();
                    }
                    _ => {}
                }
                (value, lty)
            }
            None => {
                //REVIEW: Can you do this?
                let alloca = self.builder.build_alloca(ty, "").unwrap();
                let ident = self.get_data(&local.ident).unwrap();
                self.values.insert(ident, (alloca.into(), ty));
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
        let alloca = self.builder.build_alloca(ty, "").unwrap();

        if let BasicValueEnum::PointerValue(ptr) = value {
            dbg!(local.ident);
            dbg!(lty);
            if lty.is_pointer_type() {
                self.builder.position_at_end(current_block);
                self.builder.build_store(alloca, value).unwrap();
            } else {
                ptr.replace_all_uses_with(alloca);
                ptr.as_instruction_value()
                    .unwrap()
                    .remove_from_basic_block();

                self.builder.position_at_end(current_block);

            }
        } else {
            self.builder.position_at_end(current_block);
            self.builder.build_store(alloca, value).unwrap();
        };

        let ident = self.get_data(&local.ident).unwrap();
        self.values.insert(ident, (alloca.into(), ty));
        None
    }

    fn traverse_item(&mut self, item: &mut Item) -> Self::ReturnType {
        match &mut item.kind {
            ItemKind::Function(func) => {
                self.traverse_function(func);
            }
            ItemKind::Struct(_) => {}
            ItemKind::Constant(_, _, _) => todo!(),
            ItemKind::Foreign(b) => {
                self.traverse_block(b);
            }
        }
        None
    }

    fn traverse_function(&mut self, function: &mut FnDecl) -> Self::ReturnType {
        if function.body.statements.is_empty() {
            return None;
        }
        let sig = &function.sig;
        let module = self.get_current_llvm_module().unwrap();
        let name = self.get_data(&function.name).unwrap();
        let func = module.get_function(&name).unwrap();
        self.values.insert_map();
        for (i, arg) in sig.args.iter().enumerate() {
            let a = func.get_nth_param(i as u32).unwrap();
            dbg!(self.to_llvm_type(&arg.1));
            let name = self.get_data(&arg.0).unwrap();
            self.values.insert(name, (a, self.to_llvm_type(&arg.1)));
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
