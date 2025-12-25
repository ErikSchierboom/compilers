use crate::parser::{parse, ParseError, Word};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::{Add, Mul};

trait Executable {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError>;
}

#[derive(Clone, Debug)]
enum Builtin {
    Dup,
    Drop,
    Swap,
    Over,
}

impl Executable for Builtin {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            Builtin::Dup { .. } => {
                let top = interpreter.pop()?;
                interpreter.push(top.clone());
                interpreter.push(top);
                Ok(())
            }
            Builtin::Drop { .. } => {
                interpreter.pop()?;
                Ok(())
            }
            Builtin::Swap { .. } => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                interpreter.push(top);
                interpreter.push(snd);
                Ok(())
            }
            Builtin::Over { .. } => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                interpreter.push(snd.clone());
                interpreter.push(top);
                interpreter.push(snd);
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    ValInt(i64),
    ValQuote(String),
    ValBlock(Vec<Word>),
    ValArray(Vec<Value>),
    ValBuiltin(Builtin),
}

impl Executable for Word {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            Word::Int { value, .. } => interpreter.push(Value::ValInt(value.clone())),
            Word::Quote { name, .. } => interpreter.push(Value::ValQuote(name.clone())),
            Word::Block { words, .. } => interpreter.push(Value::ValBlock(words.clone())),
            Word::Call { name, .. } => {
                match interpreter.get_variable(name)? {
                    Value::ValBlock(words) => {
                        for word in words {
                            word.execute(interpreter)?
                        }
                    }
                    Value::ValBuiltin(builtin) => builtin.execute(interpreter)?,
                    value => interpreter.push(value)
                }
            }
            Word::Array { words, .. } => interpreter.push_array(words)?,
            Word::Add { .. } => interpreter.binary_int_op(i64::add)?,
            Word::Mul { .. } => interpreter.binary_int_op(i64::mul)?,
            Word::Read { .. } => {
                let name = match interpreter.pop()? {
                    Value::ValQuote(name) => name,
                    _ => return Err(RuntimeError::ExpectedQuote)
                };
                let variable = interpreter.get_variable(&name)?;
                interpreter.push(variable.clone())
            }
            Word::Write { .. } => {
                let name = match interpreter.pop()? {
                    Value::ValQuote(name) => name,
                    _ => return Err(RuntimeError::ExpectedQuote)
                };
                let value = interpreter.pop()?;
                interpreter.set_variable(name, value)?
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    ArrayHasNonNumericElement,
    EmptyStack,
    WordAlreadyExists,
    UnknownWord(String),
    UnsupportedOperands,
    ExpectedQuote,
    ArrayHasNegativeStackEffect,
}

impl From<ParseError> for RuntimeError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

struct Interpreter {
    words: Vec<Word>,
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
}

impl Interpreter {
    fn new(words: Vec<Word>) -> Self {
        Self {
            words,
            stack: Vec::new(),
            variables: HashMap::from([
                ("dup".into(), Value::ValBuiltin(Builtin::Dup)),
                ("drop".into(), Value::ValBuiltin(Builtin::Drop)),
                ("swap".into(), Value::ValBuiltin(Builtin::Swap)),
                ("over".into(), Value::ValBuiltin(Builtin::Over)),
            ]),
        }
    }

    fn run(mut self) -> Result<Vec<Value>, RuntimeError> {
        for word in std::mem::take(&mut self.words) {
            word.execute(&mut self)?
        }

        Ok(self.stack)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    fn binary_int_op(&mut self, f: impl Fn(i64, i64) -> i64) -> Result<(), RuntimeError> {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(l), Value::ValInt(r)) => {
                self.push(Value::ValInt(f(l, r)));
                Ok(())
            }
            (Value::ValInt(i), Value::ValArray(mut arr)) |
            (Value::ValArray(mut arr), Value::ValInt(i)) => {
                for arr_val in arr.iter_mut() {
                    match arr_val {
                        Value::ValInt(arr_val_i) => *arr_val_i = f(*arr_val_i, i),
                        Value::ValArray(_) => todo!("support nested arrays"),
                        _ => return Err(RuntimeError::ArrayHasNonNumericElement)
                    }
                }

                self.push(Value::ValArray(arr));
                Ok(())
            }
            _ => Err(RuntimeError::UnsupportedOperands)
        }
    }

    fn push_array(&mut self, words: &Vec<Word>) -> Result<(), RuntimeError> {
        let stack_size_before = self.stack.len();

        for word in words {
            word.execute(self)?
        }

        if self.stack.len() < stack_size_before {
            return Err(RuntimeError::ArrayHasNegativeStackEffect);
        }

        let elements = self.stack.drain(stack_size_before..).collect();
        self.push(Value::ValArray(elements));
        Ok(())
    }

    fn get_variable(&mut self, name: &String) -> Result<Value, RuntimeError> {
        self.variables.get(name).ok_or_else(|| RuntimeError::UnknownWord(name.clone())).cloned()
    }

    fn set_variable(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        match self.variables.entry(name) {
            Entry::Occupied(_) => Err(RuntimeError::WordAlreadyExists),
            Entry::Vacant(e) => {
                e.insert(value);
                Ok(())
            }
        }
    }
}

pub fn interpret(code: &str) -> Result<Vec<Value>, RuntimeError> {
    let words = parse(code)?;
    let interpreter = Interpreter::new(words);
    interpreter.run()
}
