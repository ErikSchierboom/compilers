use crate::interpreter::{Builtin, RuntimeError, Value};
use std::collections::HashMap;

pub struct Environment {
    pub stack: Vec<Value>,
    bindings: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            bindings: HashMap::from([
                ("lt".to_string(), Value::Builtin(Builtin::Less)),
                ("le".to_string(), Value::Builtin(Builtin::LessEqual)),
                ("gt".to_string(), Value::Builtin(Builtin::Greater)),
                ("ge".to_string(), Value::Builtin(Builtin::GreaterEqual)),
                ("dup".to_string(), Value::Builtin(Builtin::Dup)),
                ("drop".to_string(), Value::Builtin(Builtin::Drop)),
            ]),
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    pub fn pop_n(&mut self, n: usize) -> Result<Vec<Value>, RuntimeError> {
        if n <= self.stack.len() {
            Ok(self.stack.drain((self.stack.len() - n)..).collect())
        } else {
            Err(RuntimeError::EmptyStack)
        }
    }

    pub fn get(&mut self, identifier: &String) -> Result<&Value, RuntimeError> {
        self.bindings.get(identifier).ok_or_else(|| RuntimeError::UnknownIdentifier(identifier.clone()))
    }
}
