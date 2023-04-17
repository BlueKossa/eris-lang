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
}

impl Default for VisitorResult<'_> {
    fn default() -> Self {
        VisitorResult { value: None }
    }
}

impl<'a> VisitorResult<'a> {
    fn new(value: Option<BasicValueEnum<'a>>) -> Self {
        VisitorResult { value }
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
        let val = if let Some(v) = local.value.as_mut() {
            self.traverse_expression_kind(v.kind.as_mut())
                .value
                .unwrap()
        } else {
            unimplemented!("No value for local variable")
        };
        let alloca = self.builder.build_alloca(val.get_type(), "ptr");
        self.builder.build_store(alloca, val);
        self.values
            .insert(local.ident, (alloca.into(), val.get_type()));
        VisitorResult::default()
    }

    fn traverse_expression_kind(&mut self, expr: &mut ExprKind<'a>) -> VisitorResult<'a> {
        match expr {
            ExprKind::Binary(op, a, b) => {
                let mut a_val = self
                    .traverse_expression_kind(a.kind.as_mut())
                    .value
                    .unwrap();
                let mut b_val = self
                    .traverse_expression_kind(b.kind.as_mut())
                    .value
                    .unwrap();
                // Dereference pointers
                if let BasicValueEnum::PointerValue(a_ptr) = a_val {
                    println!("POINTER HERE: A");
                    a_val = self
                        .builder
                        .build_load(self.context.i32_type(), a_ptr, "load")
                }
                if let BasicValueEnum::PointerValue(b_ptr) = b_val {
                    println!("POINTER HERE: B");
                    b_val = self
                        .builder
                        .build_load(self.context.i32_type(), b_ptr, "load");
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

                        return VisitorResult::new(Some(res.into()));
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

                        return VisitorResult::new(Some(res));
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

                return VisitorResult::new(Some(res.into()));
            }
            ExprKind::Literal(lit) => match lit.kind {
                LiteralKind::Int(i) => {
                    let int = self.context.i32_type().const_int(i as u64, false);
                    return VisitorResult::new(Some(int.into()));
                }
                LiteralKind::Float(f) => {
                    let float = self.context.f32_type().const_float(f);
                    return VisitorResult::new(Some(float.into()));
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
                    return VisitorResult::new(Some(ptr.into()));
                }
                LiteralKind::Bool(b) => {
                    let bool = self.context.bool_type().const_int(b as u64, false);
                    return VisitorResult::new(Some(bool.into()));
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
                let mut val = self
                    .traverse_expression_kind(val.kind.as_mut())
                    .value
                    .unwrap();

                if let BasicValueEnum::PointerValue(ptr) = val {
                    val = self.builder.build_load(self.context.i32_type(), ptr, "load");
                }

                self.builder.build_store(var, val);
                return VisitorResult::default();
            }
            ExprKind::Call(fn_name, args) => {
                let fn_val = self.module.get_function(fn_name).unwrap();
                let param_len = fn_val.count_params();
                let mut a: Vec<BasicMetadataValueEnum> = Vec::with_capacity(param_len as usize);
                for arg in args {
                    let val = if let ExprKind::Var(id) = *arg.kind {
                        let (ptr, ty) = self.values.get(id).unwrap();
                        if let BasicValueEnum::PointerValue(ptr) = ptr {
                            let ptr = self.builder.build_load(*ty, *ptr, "load");
                            ptr.into()
                        } else {
                            *ptr
                        }
                    } else {
                        self.traverse_expression_kind(arg.kind.as_mut())
                            .value
                            .unwrap()
                    };
                    a.push(val.into());
                }
                let call = self.builder.build_call(fn_val, &a, "call");
                let basic_val = call.try_as_basic_value().left();
                return VisitorResult::new(basic_val);
            }
            ExprKind::Var(var) => {
                let (ptr, ty) = self.values.get(var).unwrap();
                return VisitorResult::new(Some(*ptr));
            }
            ExprKind::Return(val) => {
                if let Some(val) = val {
                    let val = self
                        .traverse_expression_kind(val.kind.as_mut())
                        .value
                        .unwrap();
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
                    let val = self
                        .traverse_expression_kind(field.kind.as_mut())
                        .value
                        .unwrap();
                    let ptr = self
                        .builder
                        .build_struct_gep(struct_ty, ptr_value, i as u32, "field")
                        .unwrap();
                    self.builder.build_store(ptr, val);
                }

                return VisitorResult::new(Some(ptr_value.into()));
            }
            ExprKind::FieldAccess(exp, f) => {
                dbg!("1");
                let ptr = self
                    .traverse_expression_kind(exp.kind.as_mut())
                    .value
                    .unwrap();
                let ptr_value;
                let expr;
                if let BasicValueEnum::PointerValue(ptr) = ptr {
                    expr = self.builder.build_load(
                        self.context.get_struct_type("Point").unwrap(),
                        ptr,
                        "load",
                    );
                    ptr_value = ptr;
                } else {
                    todo!();
                }
                let struct_ty = self.context.get_struct_type("Point").unwrap();
                dbg!("3");
                let field_index = if let ExprKind::Var(s) = *exp.kind {
                    dbg!("4");
                    println!("s: {:?}", self.structs);
                    let struct_ = self.structs.get("Point").unwrap();
                    let index = struct_.get(*f).unwrap();
                    dbg!("5");
                    *index
                } else {
                    todo!()
                };
                dbg!("6");
                let ptr = self
                    .builder
                    .build_struct_gep(struct_ty, ptr_value, field_index as u32, "field")
                    .unwrap();
                dbg!("7");
                return VisitorResult::new(Some(ptr.into()));
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
                t => unimplemented!("{:?}", t),
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
        for a in &sig.args {
            let arg: BasicMetadataTypeEnum = match a.1 {
                Type::I32 => self.context.i32_type().into(),
                Type::F32 => self.context.f32_type().into(),
                t => unimplemented!("{:?}", t),
            };
            args.push(arg);
        }
        let fn_ty = match sig.ret {
            Type::I32 => self.context.i32_type().fn_type(&args, false),
            Type::F32 => self.context.f32_type().fn_type(&args, false),
            Type::Void => {
                void = true;
                self.context.void_type().fn_type(&args, false)
            }
            t => unimplemented!("{:?}", t),
        };
        let func = self.module.add_function(name, fn_ty, None);
        for (i, arg) in sig.args.iter().enumerate() {
            let a = func.get_nth_param(i as u32).unwrap();
            a.set_name(arg.0);
            self.values.insert(arg.0, (a, a.get_type()));
        }
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.traverse_block(&mut block.to_owned());
        if void {
            self.builder.build_return(None);
        }
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
        let mut command = Command::new("clang");
        command.arg(path).arg("-o").arg("main");
        let r = command.output().unwrap();
        println!("{}", String::from_utf8(r.stdout).unwrap());
    }
}
