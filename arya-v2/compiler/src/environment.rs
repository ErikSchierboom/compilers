use crate::interpreter::{Array, Binding, Builtin, InterpretResult, RuntimeError, Value};
use std::collections::HashMap;

pub struct Environment {
    pub stack: Vec<Value>,
    bindings: HashMap<String, Binding>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            bindings: HashMap::from([
                ("max".to_string(), Binding::Builtin(Builtin::Max)),
                ("min".to_string(), Binding::Builtin(Builtin::Min)),
                ("abs".to_string(), Binding::Builtin(Builtin::Abs)),
                ("dup".to_string(), Binding::Builtin(Builtin::Dup)),
                ("swap".to_string(), Binding::Builtin(Builtin::Swap)),
            ]),
        }
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    pub fn pop_n(&mut self, n: usize) -> InterpretResult<Vec<Value>> {
        if n <= self.stack.len() {
            Ok(self.stack.drain((self.stack.len() - n)..).collect())
        } else {
            Err(RuntimeError::EmptyStack)
        }
    }

    pub fn execute_monadic<TValueOp>(
        &mut self,
        value_op: TValueOp,
    ) -> InterpretResult where
        TValueOp: Fn(Value, &Self) -> InterpretResult<Value>,
    {
        let a = self.pop()?;
        self.push(value_op(a, self)?);
        Ok(())
    }

    pub fn execute_dyadic<TValueOp>(
        &mut self,
        value_op: TValueOp,
    ) -> InterpretResult
    where
        TValueOp: Fn(Value, Value, &Self) -> InterpretResult<Value>,
    {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(value_op(a, b, self)?);
        Ok(())
    }

    pub fn execute_monadic_num_op<TNumOp>(
        &mut self,
        num_op: TNumOp,
    ) -> InterpretResult
    where
        TNumOp: Fn(f64) -> f64,
    {
        self.execute_monadic(|value, _| {
            match value {
                Value::Number(number) => Ok(Value::Number(num_op(number))),
                Value::Array(Array::Number(mut array)) => {
                    for element in &mut array.elements {
                        *element = num_op(*element);
                    }
                    Ok(Value::Array(Array::Number(array)))
                }
                Value::Array(Array::Empty) => Ok(value),
                _ => Err(RuntimeError::UnsupportedArgumentTypes)
            }
        })
    }

    pub fn execute_dyadic_op<TNumOp, TCharOp>(
        &mut self,
        num_op: TNumOp,
        char_op: Option<TCharOp>,
    ) -> InterpretResult
    where
        TNumOp: Fn(f64, f64) -> f64,
        TCharOp: Fn(u8, u8) -> u8,
    {
        let right_val = self.pop()?;
        let left_val = self.pop()?;

        match (left_val, right_val, char_op) {
            (Value::Number(l), Value::Number(r), _) => {
                self.push(Value::Number(num_op(l, r)));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(Array::Number(mut array)), _) => {
                for elem in &mut array.elements {
                    *elem = num_op(scalar, *elem);
                }
                self.push(Value::Array(Array::Number(array)));
                Ok(())
            }

            (Value::Array(Array::Number(mut array)), Value::Number(scalar), _) => {
                for elem in &mut array.elements {
                    *elem = num_op(*elem, scalar);
                }
                self.push(Value::Array(Array::Number(array)));
                Ok(())
            }

            (Value::Array(Array::Number(mut left)), Value::Array(Array::Number(right)), _) => {
                if left.shape != right.shape {
                    return Err(RuntimeError::IncompatibleArrayShapes);
                }
                for (l, r) in left.elements.iter_mut().zip(&right.elements) {
                    *l = num_op(*l, *r);
                }
                self.push(Value::Array(Array::Number(left)));
                Ok(())
            }

            (Value::Char(c), Value::Number(number), Some(char_op)) => {
                self.push(Value::Char(char_op(c as u8, number as u8) as char));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(Array::Char(mut array)), Some(char_op))
            | (Value::Array(Array::Char(mut array)), Value::Number(scalar), Some(char_op)) => {
                for char in &mut array.elements {
                    *char = char_op(*char as u8, scalar as u8) as char;
                }
                self.push(Value::Array(Array::Char(array)));
                Ok(())
            }

            (Value::Array(Array::Char(mut chars)), Value::Array(Array::Number(numbers)), Some(char_op)) => {
                if chars.shape != numbers.shape {
                    return Err(RuntimeError::IncompatibleArrayShapes);
                }

                for (char, number) in chars.elements.iter_mut().zip(&numbers.elements) {
                    *char = char_op(*char as u8, *number as u8) as char;
                }

                self.push(Value::Array(Array::Char(chars)));
                Ok(())
            }

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }

    pub fn get(&self, identifier: &String) -> InterpretResult<&Binding> {
        self.bindings.get(identifier).ok_or_else(|| RuntimeError::UnknownIdentifier(identifier.clone()))
    }
}
