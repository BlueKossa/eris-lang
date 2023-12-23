use std::{collections::BTreeMap, rc::Rc};

use crate::{parser::{ast::{blocks::Block, items::Item, statements::Statement}, parser::Parser}, lexer::lexer::Lexer, span::Span, semantic::semantic_visitor::SemanticVisitor};

use inkwell::module::Module as LLVMModule;

pub struct ModuleMap<'a> {
    pub(crate) modules: Vec<Module<'a>>,
    pub(crate) current_module: String,
    module_count: usize,
}

impl<'a> ModuleMap<'a> {
    pub fn new() -> Self {
        ModuleMap {
            modules: Vec::new(),
            current_module: String::new(),
            module_count: 0,
        }
    }

    pub fn add_module(&mut self, path: &str, llvm_module: LLVMModule<'a>) -> usize {
        let module = Module::new(path, llvm_module, self.module_count);
        self.module_count += 1;
        self.modules.push(module);
        self.module_count - 1
    }

    pub fn get_module(&self, idx: usize) -> Option<&Module<'a>> {
        self.modules.get(idx)
    }
}

pub struct Module<'a> {
    pub(crate) ast: Block,
    pub(crate) llvm_module: LLVMModule<'a>,
    pub(crate) name: String,
    pub(crate) source: String,
    idx: usize,
}

impl<'a> Module<'a> {
    pub fn new(path: &str, llvm_module: LLVMModule<'a>, idx: usize) -> Self {
        println!("Compiling {}", path);
        let source = std::fs::read_to_string(path).unwrap();
        let lexer = Lexer::new(&source);
        let mut parser = Parser::new(lexer, &source);
        let ast = parser.into_iter()
            .map(|stmt| stmt.unwrap())
            .collect::<Vec<Statement>>();
        let mut ast = Block {
            statements: ast,
        };
        Module {
            ast,
            name: path.to_string(),
            llvm_module,
            source,
            idx,
        }
    }

    pub fn get_data(&self, span: &Span) -> Rc<str> {
        span.data(&self.source).into()
    }
}
