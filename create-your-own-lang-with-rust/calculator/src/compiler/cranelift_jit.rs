use std::mem;
use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::{AbiParam, FunctionBuilderContext, Ieee32, Imm64, InstBuilder, Value};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use crate::{Compile, Node, Operator, Result};

#[derive(Debug)]
pub enum ReturnValue {
    Int(i32),
    Float(f32)
}

pub struct CraneliftJit;

impl Compile for CraneliftJit {
    type Output = Result<Vec<ReturnValue>>;

    fn from_ast(ast: Vec<Node>) -> Self::Output {
        let mut values = Vec::new();

        let i32 = cranelift::prelude::types::I32;
        let f32 = cranelift::prelude::types::F32;

        for node in ast {
            let jit_builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
            let mut jit_module = JITModule::new(jit_builder);
            let mut ctx = jit_module.make_context();
            let mut func_ctx = FunctionBuilderContext::new();
            let mut function_builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

            let entry_block = function_builder.create_block();
            function_builder.switch_to_block(entry_block);
            function_builder.seal_block(entry_block);

            let mut recursive_builder = RecursiveBuilder::new(i32, f32, &mut function_builder, &mut jit_module);
            match recursive_builder.build(&node) {
                BuildValue::Int(value) => {
                    function_builder.ins().return_(&[value]);
                    let sig = &mut function_builder.func.signature;
                    sig.returns.push(AbiParam::new(i32));
                }
                BuildValue::Float(value) => {
                    function_builder.ins().return_(&[value]);
                    let sig = &mut function_builder.func.signature;
                    sig.returns.push(AbiParam::new(f32));
                }
            }

            function_builder.finalize();

            let id = jit_module
                .declare_function("jit_add", Linkage::Export, &ctx.func.signature)
                .map_err(|e| e.to_string())
                .unwrap();

            // Define the function to jit. This finishes compilation, although
            // there may be outstanding relocations to perform. Currently, jit
            // cannot finish relocations until all functions to be called are
            // defined. For this toy demo for now, we'll just finalize the
            // function below.
            jit_module
                .define_function(id, &mut ctx)
                .map_err(|e| e.to_string())
                .unwrap();

            // Now that compilation is finished, we can clear out the context state.
            jit_module.clear_context(&mut ctx);

            let code_ptr = jit_module.get_finalized_function(id);


            // Cast the raw pointer to a typed function pointer. This is unsafe, because
            // this is the critical point where you have to trust that the generated code
            // is safe to be called.
            unsafe {
                let code_fn = mem::transmute::<_, fn() -> i32>(code_ptr);
                // And now we can call it!
                values.push(ReturnValue::Int(code_fn()))
            }
        }

        Ok(values)
    }
}

pub enum BuildValue {
    Int(Value),
    Float(Value)
}

struct RecursiveBuilder<'a> {
    i32_type: cranelift::prelude::Type,
    f32_type: cranelift::prelude::Type,
    builder: &'a mut FunctionBuilder<'a>,
    module: &'a mut JITModule,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: cranelift::prelude::Type, f32_type: cranelift::prelude::Type, builder: &'a mut FunctionBuilder<'a>, module: &'a mut JITModule,) -> Self {
        Self { i32_type, f32_type, builder, module }
    }

    pub fn build(&mut self, ast: &Node) -> BuildValue {
        match ast {
            Node::Int(n) => {
                let n: Imm64 = (*n as i64).into();
                BuildValue::Int(self.builder.ins().iconst(self.i32_type, n))
            },
            Node::Float(n) => {
                let n: Ieee32 = (*n).into();
                BuildValue::Float(self.builder.ins().f32const(n))
            },
            Node::UnaryExpr { op, child } => {
                let child = self.build(child);
                match (op, &child) {
                    (Operator::Minus, BuildValue::Int(v)) => BuildValue::Int(self.builder.ins().ineg(*v)),
                    (Operator::Minus, BuildValue::Float(v)) => BuildValue::Float(self.builder.ins().fneg(*v)),
                    (Operator::Plus, _) => child,
                    _ => unreachable!()
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => {
                let left = self.build(lhs);
                let right = self.build(rhs);

                match (left, op, right) {
                    (BuildValue::Int(l), Operator::Plus, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .ins()
                            .iadd(l, r)),
                    (BuildValue::Float(l), Operator::Plus, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .ins()
                            .fadd(l, r)),
                    (BuildValue::Int(l), Operator::Minus, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .ins()
                            .isub(l, r)),
                    (BuildValue::Float(l), Operator::Minus, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .ins()
                            .fsub(l, r)),
                    (BuildValue::Int(l), Operator::Multiply, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .ins()
                            .imul(l, r)),
                    (BuildValue::Float(l), Operator::Multiply, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .ins()
                            .fmul(l, r)),
                    (BuildValue::Int(l), Operator::Divide, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .ins()
                            .srem(l, r)),
                    (BuildValue::Float(l), Operator::Divide, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .ins()
                            .fdiv(l, r)),
                    _ => panic!("Unsupported operands")
                }
            }
        }
    }
}
