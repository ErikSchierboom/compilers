use cranelift::frontend::FunctionBuilder;
use cranelift::prelude::{AbiParam, FunctionBuilderContext, InstBuilder, Value};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;
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
            let jit_module = JITModule::new(jit_builder);
            let mut ctx = jit_module.make_context();
            let mut func_ctx = FunctionBuilderContext::new();
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

            let entry_block = builder.create_block();
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);


            let sig = &mut builder.func.signature;
            sig.returns.push(AbiParam::new(i32));

            let recursive_builder = RecursiveBuilder::new()
        }

        Ok(values)
    }
}

struct RecursiveBuilder<'a> {
    i32_type: cranelift::prelude::Type,
    f32_type: cranelift::prelude::Type,
    builder: &'a FunctionBuilder<'a>,
    module: &'a mut JITModule,
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: cranelift::prelude::Type, f32_type: cranelift::prelude::Type, builder: &'a FunctionBuilder<'a>, module: &'a mut JITModule,) -> Self {
        Self { i32_type, f32_type, builder, module }
    }

    pub fn build(&mut self, ast: &Node) -> Value {
        match ast {
            Node::Int(n) => self.builder.ins().iconst(self.i32_type, n.into()),
            Node::Float(n) => self.builder.ins().f32const(n.into()),
            Node::UnaryExpr { op, child } => {
                let child = self.build(child);
                match (op, &child) {
                    (Operator::Minus, crate::compiler::jit::BuildValue::Int(v)) => crate::compiler::jit::BuildValue::Int(v.const_neg()),
                    (Operator::Minus, crate::compiler::jit::BuildValue::Float(v)) => {
                        let neg = self
                            .builder
                            .build_float_mul(*v, self.f32_type.const_float(-1f64), "neg_temp")
                            .unwrap();
                        crate::compiler::jit::BuildValue::Float(neg)
                    }
                    (Operator::Plus, _) => child,
                    _ => unreachable!()
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => {
                let left = self.build(lhs);
                let right = self.build(rhs);

                match (left, op, right) {
                    (crate::compiler::jit::BuildValue::Int(l), Operator::Plus, crate::compiler::jit::BuildValue::Int(r)) =>
                        crate::compiler::jit::BuildValue::Int(self
                            .builder
                            .build_int_add(l, r, "plus_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Float(l), Operator::Plus, crate::compiler::jit::BuildValue::Float(r)) =>
                        crate::compiler::jit::BuildValue::Float(self
                            .builder
                            .build_float_add(l, r, "plus_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Int(l), Operator::Minus, crate::compiler::jit::BuildValue::Int(r)) =>
                        crate::compiler::jit::BuildValue::Int(self
                            .builder
                            .build_int_sub(l, r, "minus_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Float(l), Operator::Minus, crate::compiler::jit::BuildValue::Float(r)) =>
                        crate::compiler::jit::BuildValue::Float(self
                            .builder
                            .build_float_sub(l, r, "minus_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Int(l), Operator::Multiply, crate::compiler::jit::BuildValue::Int(r)) =>
                        crate::compiler::jit::BuildValue::Int(self
                            .builder
                            .build_int_mul(l, r, "mul_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Float(l), Operator::Multiply, crate::compiler::jit::BuildValue::Float(r)) =>
                        crate::compiler::jit::BuildValue::Float(self
                            .builder
                            .build_float_mul(l, r, "mul_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Int(l), Operator::Divide, crate::compiler::jit::BuildValue::Int(r)) =>
                        crate::compiler::jit::BuildValue::Int(self
                            .builder
                            .build_int_signed_div(l, r, "mul_temp")
                            .unwrap()),
                    (crate::compiler::jit::BuildValue::Float(l), Operator::Divide, crate::compiler::jit::BuildValue::Float(r)) =>
                        crate::compiler::jit::BuildValue::Float(self
                            .builder
                            .build_float_div(l, r, "mul_temp")
                            .unwrap()),
                    _ => panic!("Unsupported operands")
                }
            }
        }
    }
}
