use inkwell::{
    builder::Builder, context::Context, execution_engine::JitFunction, types::IntType,
    values::AnyValue, values::IntValue, values::FloatValue, OptimizationLevel,
};
use inkwell::types::FloatType;
use crate::{Compile, Node, Operator, Result};

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Float(f32)
}

type JitFuncF32 = unsafe extern "C" fn() -> f32;
type JitFuncI32 = unsafe extern "C" fn() -> i32;

// ANCHOR: jit_ast
pub struct Jit;

impl Compile for Jit {
    type Output = Result<Vec<Value>>;

    fn from_ast(ast: Vec<Node>) -> Self::Output {
        let context = Context::create();
        let module = context.create_module("calculator");

        let builder = context.create_builder();

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();

        let i32_type = context.i32_type();
        let f32_type = context.f32_type();
        let fn_type = f32_type.fn_type(&[], false);



        let mut values = Vec::new();

        for node in ast {
            let function = module.add_function("jit", fn_type, None);
            let basic_block = context.append_basic_block(function, "entry");

            builder.position_at_end(basic_block);

            let recursive_builder = RecursiveBuilder::new(i32_type, f32_type, &builder);
            let return_value = recursive_builder.build(&node);
            let value = match return_value {
                BuildValue::Int(int_return_value) => {
                    let _ = builder.build_return(Some(&int_return_value));

                    println!(
                        "Generated LLVM IR: {}",
                        function.print_to_string().to_string()
                    );

                    unsafe {
                        let jit_function: JitFunction<JitFuncI32> = execution_engine.get_function("jit").unwrap();
                        Value::Int(jit_function.call())
                    }
                },
                BuildValue::Float(float_return_value) => {
                    let _ = builder.build_return(Some(&float_return_value));

                    println!(
                        "Generated LLVM IR: {}",
                        function.print_to_string().to_string()
                    );

                    unsafe {
                        let jit_function: JitFunction<JitFuncF32> = execution_engine.get_function("jit").unwrap();
                        Value::Float(jit_function.call())
                    }
                }
            };
            values.push(value)
        }

        Ok(values)
    }
}
// ANCHOR_END: jit_ast

// ANCHOR: jit_recursive_builder
struct RecursiveBuilder<'a> {
    i32_type: IntType<'a>,
    f32_type: FloatType<'a>,
    builder: &'a Builder<'a>,
}

pub enum BuildValue<'a> {
    Int(IntValue<'a>),
    Float(FloatValue<'a>)
}

impl<'a> RecursiveBuilder<'a> {
    pub fn new(i32_type: IntType<'a>, f32_type: FloatType<'a>, builder: &'a Builder) -> Self {
        Self { i32_type, f32_type, builder }
    }
    pub fn build(&self, ast: &Node) -> BuildValue<'a> {
        match ast {
            Node::Int(n) => BuildValue::Int(self.i32_type.const_int(*n as u64, true)),
            Node::Float(n) => BuildValue::Float(self.f32_type.const_float(*n as f64)),
            Node::UnaryExpr { op, child } => {
                let child = self.build(child);
                match (op, &child) {
                    (Operator::Minus, BuildValue::Int(v)) => BuildValue::Int(v.const_neg()),
                    (Operator::Minus, BuildValue::Float(v)) => {
                        let neg = self
                            .builder
                            .build_float_mul(*v, self.f32_type.const_float(-1f64), "neg_temp")
                            .unwrap();
                        BuildValue::Float(neg)
                    }
                    (Operator::Plus, _) => child,
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => {
                let left = self.build(lhs);
                let right = self.build(rhs);

                match (left, op, right) {
                    (BuildValue::Int(l), Operator::Plus, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .build_int_add(l, r, "plus_temp")
                            .unwrap()),
                    (BuildValue::Float(l), Operator::Plus, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .build_float_add(l, r, "plus_temp")
                            .unwrap()),
                    (BuildValue::Int(l), Operator::Minus, BuildValue::Int(r)) =>
                        BuildValue::Int(self
                            .builder
                            .build_int_sub(l, r, "minus_temp")
                            .unwrap()),
                    (BuildValue::Float(l), Operator::Minus, BuildValue::Float(r)) =>
                        BuildValue::Float(self
                            .builder
                            .build_float_sub(l, r, "minus_temp")
                            .unwrap()),
                    _ => panic!("Unsupported operands")
                }
            }
        }
    }
}
// ANCHOR_END: jit_recursive_builder

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        assert!(matches!(Jit::from_source("1 + 2").unwrap().first().unwrap(), Value::Int(3)));
        assert!(matches!(Jit::from_source("2 + (2 - 1)").unwrap().first().unwrap(), Value::Int(3)));
        assert!(matches!(Jit::from_source("(2 + 3) - 1").unwrap().first().unwrap(), Value::Int(4)));
        assert!(matches!(Jit::from_source("1 + ((2 + 3) - (2 + 3))").unwrap().first().unwrap(), Value::Int(1)));
        assert!(matches!(Jit::from_source("(1 + 2)").unwrap().first().unwrap(), Value::Int(3)));
        // parser fails
        // assert_eq!(Jit::from_source("2 + 3 - 1").unwrap(), 4);
    }
}
