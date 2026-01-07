#![allow(clippy::only_used_in_recursion)]
use crate::{Compile, Node, Operator, Result};

// ANCHOR: interpreter
pub struct Interpreter;

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Float(f32)
}

impl Compile for Interpreter {
    type Output = Result<Vec<Value>>;

    fn from_ast(ast: Vec<Node>) -> Self::Output {
        let mut ret = Vec::new();
        let evaluator = Eval::new();
        for node in ast {
            ret.push(evaluator.eval(&node));
        }
        Ok(ret)
    }
}
// ANCHOR_END: interpreter

// ANCHOR: interpreter_recursive
struct Eval;

impl Eval {
    pub fn new() -> Self {
        Self
    }
    // ANCHOR: interpreter_eval
    pub fn eval(&self, node: &Node) -> Value {
        match node {
            Node::Int(n) => Value::Int(*n),
            Node::Float(n) => Value::Float(*n),
            Node::UnaryExpr { op, child } => {
                let child = self.eval(child);
                match (op, &child) {
                    (Operator::Plus, _) => child,
                    (Operator::Minus, Value::Int(i)) => Value::Int(-i),
                    (Operator::Minus, Value::Float(i)) => Value::Float(-i),
                    _ => unreachable!()
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => {
                let lhs_ret = self.eval(lhs);
                let rhs_ret = self.eval(rhs);

                match (lhs_ret, op, rhs_ret) {
                    (Value::Int(l), Operator::Plus, Value::Int(r)) => Value::Int(l + r),
                    (Value::Int(l), Operator::Minus, Value::Int(r)) => Value::Int(l - r),
                    (Value::Int(l), Operator::Multiply, Value::Int(r)) => Value::Int(l * r),
                    (Value::Int(l), Operator::Divide, Value::Int(r)) => Value::Int(l / r),
                    (Value::Float(l), Operator::Plus, Value::Float(r)) => Value::Float(l + r),
                    (Value::Float(l), Operator::Minus, Value::Float(r)) => Value::Float(l - r),
                    (Value::Float(l), Operator::Multiply, Value::Float(r)) => Value::Float(l * r),
                    (Value::Float(l), Operator::Divide, Value::Float(r)) => Value::Float(l / r),
                    _ => panic!("Unsupported operands")
                }
            }
        }
    }
    // ANCHOR_END: interpreter_eval
}
// ANCHOR_END: interpreter_recursive

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        assert!(matches!(Interpreter::from_source("1").unwrap().first().unwrap(), Value::Int(1)));
        assert!(matches!(Interpreter::from_source("1.1").unwrap().first().unwrap(), Value::Float(1.1)));
        assert!(matches!(Interpreter::from_source("1 + 2").unwrap().first().unwrap(), Value::Int(3)));
        // assert_eq!(Interpreter::source("(1 + 2)").unwrap() as i32, 3);
        assert!(matches!(Interpreter::from_source("2 + (2 - 1)").unwrap().first().unwrap(), Value::Int(3)));
        assert!(matches!(Interpreter::from_source("(2 + 3) - 1").unwrap().first().unwrap(), Value::Int(4)));
        assert!(matches!(
            Interpreter::from_source("1 + ((2 + 3) - (2 + 3))").unwrap().first().unwrap(),
            Value::Int(1
        )));
    }
}
