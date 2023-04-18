use std::{collections::HashMap, path::Path, process::Command};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum},
    AddressSpace, OptimizationLevel,
};

use crate::parser::ast::{
    blocks::Block,
    expressions::{Expr, ExprKind},
    functions::FnDecl,
    items::{Item, ItemKind},
    literals::LiteralKind,
    locals::Local,
    operators::{BinaryOp, UnaryOp},
    statements::Statement,
    structs::Struct,
    types::Type,
};

pub struct Visitor<'a> {
    builder: Builder<'a>,
    context: &'a Context,
    pub module: Module<'a>,
    values: HashMap<&'a str, (BasicValueEnum<'a>, BasicTypeEnum<'a>)>,
    structs: HashMap<&'a str, HashMap<&'a str, usize>>,
}

pub struct VisitorResult<'a> {
    value: Option<BasicValueEnum<'a>>,
    ty: Option<BasicTypeEnum<'a>>,
}

impl Default for VisitorResult<'_> {
    fn default() -> Self {
        VisitorResult {
            value: None,
            ty: None,
        }
    }
}

impl<'a> VisitorResult<'a> {
    fn new(value: Option<BasicValueEnum<'a>>, ty: Option<BasicTypeEnum<'a>>) -> Self {
        VisitorResult { value, ty }
    }

    fn new_no_type(value: Option<BasicValueEnum<'a>>) -> Self {
        VisitorResult { value, ty: None }
    }
}

impl<'a> Visitor<'a> {
    pub fn new(context: &'a Context, module_name: &str) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(module_name);
        Visitor {
            builder,
            context,
            module: module,
            values: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    fn traverse_block(&mut self, block: &mut Block<'a>) -> VisitorResult<'a> {
        for statement in block.statements.iter_mut() {
            self.traverse_statement(statement);
        }
        VisitorResult::default()
    }

    fn traverse_statement(&mut self, stmt: &mut Statement<'a>) -> VisitorResult<'a> {
        match stmt {
            Statement::Item(item) => self.traverse_item(item),
            Statement::Expression(ref mut expr) => {
                self.traverse_expression_kind(expr.kind.as_mut())
            }
            Statement::Local(local) => self.traverse_local(local),
        };
        VisitorResult::default()
    }

    fn traverse_local(&mut self, local: &mut Local<'a>) -> VisitorResult<'a> {
        let (mut val, ty) = if let Some(v) = local.value.as_mut() {
            let res = self.traverse_expression_kind(v.kind.as_mut());
            (res.value.unwrap(), res.ty)
        } else {
            unimplemented!("No value for local variable")
        };
        // Dereference pointers
        if let BasicValueEnum::PointerValue(ptr) = val {
            val = self.builder.build_load(ty.unwrap(), ptr, "load")
        }

        let alloca = self.builder.build_alloca(val.get_type(), "ptr");
        self.builder.build_store(alloca, val);
        self.values
            .insert(local.ident, (alloca.into(), val.get_type()));
        VisitorResult::default()
    }

    fn traverse_expression_kind(&mut self, expr: &mut ExprKind<'a>) -> VisitorResult<'a> {
        match expr {
            ExprKind::Binary(op, a, b) => {
                let a = self.traverse_expression_kind(a.kind.as_mut());
                let b = self.traverse_expression_kind(b.kind.as_mut());
                let mut a_val = a.value.unwrap();
                let mut b_val = b.value.unwrap();

                // Dereference pointers
                if let BasicValueEnum::PointerValue(a_ptr) = a_val {
                    a_val = self.builder.build_load(a.ty.unwrap(), a_ptr, "load")
                }
                if let BasicValueEnum::PointerValue(b_ptr) = b_val {
                    b_val = self.builder.build_load(b.ty.unwrap(), b_ptr, "load");
                }

                match (a_val, b_val) {
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

                        return VisitorResult::new_no_type(Some(res.into()));
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

                        return VisitorResult::new_no_type(Some(res));
                    }
                    t => todo!("{:?}", t),
                }
            }
            ExprKind::Unary(op, expr) => {
                let expr_res = self
                    .traverse_expression_kind(expr.kind.as_mut())
                    .value
                    .unwrap();
                let res: BasicValueEnum = match op {
                    UnaryOp::Negate => match expr_res {
                        BasicValueEnum::IntValue(i) => self.builder.build_int_neg(i, "neg").into(),
                        BasicValueEnum::FloatValue(f) => {
                            self.builder.build_float_neg(f, "neg").into()
                        }
                        _ => todo!(),
                    },
                    UnaryOp::Not => match expr_res {
                        BasicValueEnum::IntValue(i) => self.builder.build_not(i, "not").into(),
                        _ => todo!(),
                    },
                };

                return VisitorResult::new(Some(res.into()), None);
            }
            ExprKind::Literal(lit) => match lit.kind {
                LiteralKind::Int(i) => {
                    let int = self.context.i32_type().const_int(i as u64, false);
                    return VisitorResult::new_no_type(Some(int.into()));
                }
                LiteralKind::Float(f) => {
                    let float = self.context.f32_type().const_float(f);
                    return VisitorResult::new_no_type(Some(float.into()));
                }
                LiteralKind::String(s) => {
                    let s_len = s.len() as u32;
                    // Remove the quotes
                    let mut esc = false;
                    let mut bytes = Vec::new();
                    let s = &s[1..s_len as usize - 1];
                    for c in s.chars() {
                        match c {
                            '\\' => {
                                esc = true;
                            }
                            'n' if esc => {
                                bytes.push(b'\n' as char);
                                esc = false;
                            }
                            c => {
                                bytes.push(c as char);
                                esc = false;
                            }
                        }
                    }
                    let string = self
                        .builder
                        .build_global_string_ptr(&bytes.into_iter().collect::<String>(), "string");
                    let ptr = string.as_pointer_value();
                    return VisitorResult::new_no_type(Some(ptr.into()));
                }
                LiteralKind::Bool(b) => {
                    let bool = self.context.bool_type().const_int(b as u64, false);
                    return VisitorResult::new_no_type(Some(bool.into()));
                }
            },
            ExprKind::If(_, _) => todo!(),
            ExprKind::While(cond, block) => {
                let start = self.builder.get_insert_block().unwrap();
                let func = start.get_parent().unwrap();
                let cond_block = self.context.append_basic_block(func, "whilecond");
                let loop_block = self.context.append_basic_block(func, "whileloop");
                let end_block = self.context.append_basic_block(func, "whileend");

                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(cond_block);

                let cond = self
                    .traverse_expression_kind(cond.kind.as_mut())
                    .value
                    .unwrap()
                    .into_int_value();

                self.builder
                    .build_conditional_branch(cond, loop_block, end_block);
                self.builder.position_at_end(loop_block);

                self.traverse_block(block);

                self.builder.build_unconditional_branch(cond_block);
                self.builder.position_at_end(end_block);

                return VisitorResult::default();
            }
            ExprKind::Assign(var, val) => {
                let var = self
                    .traverse_expression_kind(var.kind.as_mut())
                    .value
                    .unwrap()
                    .into_pointer_value();
                let rhs = self.traverse_expression_kind(val.kind.as_mut());
                let mut val = rhs.value.unwrap();

                if let BasicValueEnum::PointerValue(ptr) = val {
                    val = self.builder.build_load(rhs.ty.unwrap(), ptr, "load");
                }

                self.builder.build_store(var, val);
                return VisitorResult::default();
            }
            ExprKind::Call(fn_name, args) => {
                let fn_val = self.module.get_function(fn_name).unwrap();
                let param_len = fn_val.count_params();
                let mut a: Vec<BasicMetadataValueEnum> = Vec::with_capacity(param_len as usize);
                for arg in args {
                    let val = match *arg.kind {
                        ExprKind::Var(id) => {
                            let (ptr, ty) = self.values.get(id).unwrap();
                            if let BasicValueEnum::PointerValue(ptr) = ptr {
                                let ptr = self.builder.build_load(*ty, *ptr, "load");
                                ptr.into()
                            } else {
                                *ptr
                            }
                        }
                        ExprKind::FieldAccess(_, _) => {
                            let res = self.traverse_expression_kind(arg.kind.as_mut());
                            if let BasicValueEnum::PointerValue(ptr) = res.value.unwrap() {
                                let ptr = self.builder.build_load(res.ty.unwrap(), ptr, "load");
                                ptr.into()
                            } else {
                                res.value.unwrap()
                            }
                        }
                        _ => self
                            .traverse_expression_kind(arg.kind.as_mut())
                            .value
                            .unwrap(),
                    };
                    a.push(val.into());
                }
                let call = self.builder.build_call(fn_val, &a, "call");
                let basic_val = call.try_as_basic_value().left();
                return VisitorResult::new(basic_val, fn_val.get_type().get_return_type());
            }
            ExprKind::Var(var) => {
                let (ptr, ty) = self.values.get(var).unwrap();

                return VisitorResult::new(Some(*ptr), Some(*ty));
            }
            ExprKind::Return(val) => {
                if let Some(val) = val {
                    let res = self.traverse_expression_kind(val.kind.as_mut());
                    let mut val = res.value.unwrap();
                    // Dereference
                    if let BasicValueEnum::PointerValue(ptr) = val {
                        val = self.builder.build_load(res.ty.unwrap(), ptr, "load");
                    }
                    self.builder.build_return(Some(&val));
                } else {
                    self.builder.build_return(None);
                }
                return VisitorResult::default();
            }
            ExprKind::StructInit(name, fields) => {
                let struct_ty = self.module.get_struct_type(name).unwrap();
                let ptr_value = self.builder.build_alloca(struct_ty, "struct");

                for (i, field) in fields.iter_mut().enumerate() {
                    let v = self
                        .traverse_expression_kind(field.kind.as_mut());
                    let mut val = v.value.unwrap();
                    if let BasicValueEnum::PointerValue(ptr) = val {
                        val = self.builder.build_load(v.ty.unwrap(), ptr, "load");
                    }
                    let ptr = self
                        .builder
                        .build_struct_gep(struct_ty, ptr_value, i as u32, "field")
                        .unwrap();
                    self.builder.build_store(ptr, val);
                }

                return VisitorResult::new(Some(ptr_value.into()), Some(struct_ty.into()));
            }
            ExprKind::FieldAccess(exp, f) => {
                let expr = self.traverse_expression_kind(exp.kind.as_mut());
                let ptr = expr.value.unwrap();
                let ty = expr.ty.unwrap();
                let ptr_value;
                let expr;
                if let BasicValueEnum::PointerValue(ptr) = ptr {
                    expr = self.builder.build_load(ty.into_struct_type(), ptr, "load");
                    ptr_value = ptr;
                } else {
                    expr = ptr;
                    ptr_value = self.builder.build_alloca(ty, "field");
                    self.builder.build_store(ptr_value, expr);
                }
                let (field_index, field_type) = match *exp.kind {
                    ExprKind::Var(_) => {
                        let struct_ty = ty.into_struct_type();
                        let field_name = struct_ty.get_name().unwrap().to_str().unwrap();
                        let field_index = *self.structs.get(field_name).unwrap().get(f).unwrap() as u32;
                        let field_type = struct_ty.get_field_type_at_index(field_index).unwrap();
                        (field_index, field_type)
                    }
                    ExprKind::FieldAccess(_, _) => {
                        let struct_ty = ty.into_struct_type();
                        let field_name = struct_ty.get_name().unwrap().to_str().unwrap();
                        let field_index = *self.structs.get(field_name).unwrap().get(f).unwrap() as u32;
                        let field_type = struct_ty.get_field_type_at_index(field_index).unwrap();
                        (field_index, field_type)
                    }
                    _ => todo!(),
                };
                let ptr = self
                    .builder
                    .build_struct_gep(ty, ptr_value, field_index, "field")
                    .unwrap();

                return VisitorResult::new(Some(ptr.into()), Some(field_type));
            }
            _ => todo!(),
        }
    }

    fn traverse_item(&mut self, item: &mut Item<'a>) -> VisitorResult<'a> {
        match item.kind {
            ItemKind::Function(ref mut func) => {
                self.traverse_function_decl(func);
            }
            ItemKind::Struct(ref mut s) => {
                self.traverse_struct_decl(s);
            }
            ItemKind::Constant(_, _, _) => {
                todo!()
            }
        }
        VisitorResult::default()
    }

    fn traverse_struct_decl(&mut self, struct_: &mut Struct<'a>) -> VisitorResult<'a> {
        let name = struct_.name;
        let mut fields: Vec<BasicTypeEnum> = Vec::with_capacity(struct_.fields.len());
        let mut struct_fields = HashMap::new();
        let mut i = 0;
        for f in &struct_.fields {
            let field: BasicTypeEnum = match f.ty {
                Type::I32 => self.context.i32_type().into(),
                Type::F32 => self.context.f32_type().into(),
                Type::Struct(s) => {
                    let ty = self.module.get_struct_type(s).unwrap();
                    ty.into()
                }
                _ => todo!(),
            };
            struct_fields.insert(f.name, i);
            i += 1;
            fields.push(field);
        }
        let st = self.context.opaque_struct_type(name);
        st.set_body(&fields, false);
        self.structs.insert(name, struct_fields);

        VisitorResult::default()
    }

    fn traverse_function_decl(&mut self, func: &mut FnDecl<'a>) -> VisitorResult<'a> {
        let sig = &func.sig;
        let name = func.name;
        let block = &func.body;
        let mut void = false;
        let mut args: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(sig.args.len());
        dbg!("FN1");
        for a in &sig.args {
            let arg: BasicMetadataTypeEnum = match a.1 {
                Type::I32 => self.context.i32_type().into(),
                Type::F32 => self.context.f32_type().into(),
                Type::Struct(name) => {
                    let struct_ty = self.module.get_struct_type(name).unwrap();
                    struct_ty.into()
                }
                t => unimplemented!("{:?}", t),
            };
            args.push(arg);
        }
        dbg!("FN2");
        let fn_ty = match sig.ret {
            Type::I32 => self.context.i32_type().fn_type(&args, false),
            Type::F32 => self.context.f32_type().fn_type(&args, false),
            Type::Void => {
                void = true;
                self.context.void_type().fn_type(&args, false)
            }
            Type::Struct(s) => {
                let struct_ty = self.module.get_struct_type(s).unwrap();
                struct_ty.fn_type(&args, false)
            }
            _ => todo!(),
        };
        dbg!("FN3");
        let func = self.module.add_function(name, fn_ty, None);
        for (i, arg) in sig.args.iter().enumerate() {
            let a = func.get_nth_param(i as u32).unwrap();
            a.set_name(arg.0);
            self.values.insert(arg.0, (a, a.get_type()));
        }
        dbg!("FN4");
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.traverse_block(&mut block.to_owned());
        if void {
            self.builder.build_return(None);
        }
        dbg!("FN5");
        VisitorResult::default()
    }

    pub fn traverse(&mut self, block: &mut Block<'a>) -> VisitorResult<'a> {
        let i32_type = self.context.i32_type();
        let fn_ty = i32_type.fn_type(&[i32_type.into()], false);
        self.module.add_function("putchar", fn_ty, None);
        let char_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let fn_ty = i32_type.fn_type(&[char_type.into()], false);
        self.module.add_function("printf", fn_ty, None);
        self.traverse_block(block)
    }

    // TODO: Move this to a better place
    pub fn generate_object_file(&self, path: &str) {
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
        let mut command = Command::new("clang");
        command.arg(path).arg("-o").arg("main");
        let r = command.output().unwrap();
    }
}
