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
        let i32 = cranelift::prelude::types::I32;
        let f32 = cranelift::prelude::types::F32;

        let jit_builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
        let mut jit_module = JITModule::new(jit_builder);
        let mut ctx = jit_module.make_context();
        let mut func_ctx = FunctionBuilderContext::new();

        let values = ast.into_iter().enumerate().map(|(idx, node)| {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let entry_block = builder.create_block();
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let result = RecursiveBuilder::build(i32, f32, &mut builder, &node);
            let is_int = matches!(result, BuildValue::Int(_));
            let value = match result { BuildValue::Int(v) | BuildValue::Float(v) => v };

            builder.ins().return_(&[value]);
            builder.func.signature.returns.push(AbiParam::new(if is_int { i32 } else { f32 }));
            builder.finalize();

            let id = jit_module.declare_function(&format!("f{}", idx), Linkage::Export, &ctx.func.signature).unwrap();
            jit_module.define_function(id, &mut ctx).unwrap();
            jit_module.clear_context(&mut ctx);
            jit_module.finalize_definitions().unwrap();

            let code_ptr = jit_module.get_finalized_function(id);
            unsafe {
                if is_int {
                    ReturnValue::Int(mem::transmute::<_, fn() -> i32>(code_ptr)())
                } else {
                    ReturnValue::Float(mem::transmute::<_, fn() -> f32>(code_ptr)())
                }
            }
        }).collect();

        Ok(values)
    }
}

pub enum BuildValue {
    Int(Value),
    Float(Value)
}

struct RecursiveBuilder;

impl RecursiveBuilder {
    pub fn build(i32_type: cranelift::prelude::Type, f32_type: cranelift::prelude::Type, builder: &mut FunctionBuilder, ast: &Node) -> BuildValue {
        match ast {
            Node::Int(n) => {
                let n: Imm64 = (*n as i64).into();
                BuildValue::Int(builder.ins().iconst(i32_type, n))
            },
            Node::Float(n) => {
                let n: Ieee32 = (*n).into();
                BuildValue::Float(builder.ins().f32const(n))
            },
            Node::UnaryExpr { op, child } => {
                let child = Self::build(i32_type, f32_type, builder, child);
                match (op, &child) {
                    (Operator::Minus, BuildValue::Int(v)) => BuildValue::Int(builder.ins().ineg(*v)),
                    (Operator::Minus, BuildValue::Float(v)) => BuildValue::Float(builder.ins().fneg(*v)),
                    (Operator::Plus, _) => child,
                    _ => unreachable!()
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => {
                let left = Self::build(i32_type, f32_type, builder, lhs);
                let right = Self::build(i32_type, f32_type, builder, rhs);

                match (left, op, right) {
                    (BuildValue::Int(l), Operator::Plus, BuildValue::Int(r)) =>
                        BuildValue::Int(builder
                            .ins()
                            .iadd(l, r)),
                    (BuildValue::Float(l), Operator::Plus, BuildValue::Float(r)) =>
                        BuildValue::Float(builder
                            .ins()
                            .fadd(l, r)),
                    (BuildValue::Int(l), Operator::Minus, BuildValue::Int(r)) =>
                        BuildValue::Int(builder
                            .ins()
                            .isub(l, r)),
                    (BuildValue::Float(l), Operator::Minus, BuildValue::Float(r)) =>
                        BuildValue::Float(builder
                            .ins()
                            .fsub(l, r)),
                    (BuildValue::Int(l), Operator::Multiply, BuildValue::Int(r)) =>
                        BuildValue::Int(builder
                            .ins()
                            .imul(l, r)),
                    (BuildValue::Float(l), Operator::Multiply, BuildValue::Float(r)) =>
                        BuildValue::Float(builder
                            .ins()
                            .fmul(l, r)),
                    (BuildValue::Int(l), Operator::Divide, BuildValue::Int(r)) =>
                        BuildValue::Int(builder
                            .ins()
                            .srem(l, r)),
                    (BuildValue::Float(l), Operator::Divide, BuildValue::Float(r)) =>
                        BuildValue::Float(builder
                            .ins()
                            .fdiv(l, r)),
                    _ => panic!("Unsupported operands")
                }
            }
        }
    }
}
