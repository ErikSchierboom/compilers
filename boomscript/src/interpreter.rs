use crate::parser::{parse, BuiltinKind, ParseError, Word};
use std::collections::HashMap;

trait Executable {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError>;
}

impl Executable for BuiltinKind {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            BuiltinKind::Dup { .. } => {
                let last = interpreter.stack.last().ok_or_else(|| RuntimeError::EmptyStack)?;
                interpreter.stack.push(last.clone());
                Ok(())
            }
            BuiltinKind::Drop { .. } => {
                interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                Ok(())
            }
            BuiltinKind::Swap { .. } => {
                let r = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                let l = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                interpreter.stack.push(r);
                interpreter.stack.push(l);
                Ok(())
            }
            BuiltinKind::Over { .. } => {
                let r = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                let l = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                interpreter.stack.push(l.clone());
                interpreter.stack.push(r);
                interpreter.stack.push(l);
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
}

impl Executable for Word {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            Word::Int { value, .. } => interpreter.stack.push(Value::ValInt(value.clone())),
            Word::Quote { name, .. } => interpreter.stack.push(Value::ValQuote(name.clone())),
            Word::Block { words, .. } => interpreter.stack.push(Value::ValBlock(words.clone())),
            Word::Builtin { kind, .. } => kind.execute(interpreter)?,
            Word::Array { words: elements, .. } => {
                let stack_size_before = interpreter.stack.len();

                for element in elements {
                    element.execute(interpreter)?
                }

                let elements = interpreter.stack.drain(stack_size_before..).collect();
                interpreter.stack.push(Value::ValArray(elements))
            }
            Word::Add { .. } => {
                let r = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                let l = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                match (l, r) {
                    (Value::ValInt(l_i), Value::ValInt(r_i)) => interpreter.stack.push(Value::ValInt(l_i + r_i)),
                    (Value::ValInt(i), Value::ValArray(mut a)) |
                    (Value::ValArray(mut a), Value::ValInt(i)) => {
                        // TODO: mutable array in-place to save on allocations

                        for value in a.iter_mut() {
                            match value {
                                Value::ValInt(vi) => *vi = *vi + i,
                                Value::ValArray(_) => todo!("support nested arrays"),
                                _ => return Err(RuntimeError::ArrayHasNonNumericElement)
                            }
                        }

                        interpreter.stack.push(Value::ValArray(a))
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }
            }
            Word::Mul { .. } => {
                let r = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                let l = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                match (l, r) {
                    (Value::ValInt(l_i), Value::ValInt(r_i)) => interpreter.stack.push(Value::ValInt(l_i * r_i)),
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }
            }
            Word::Read { variable, .. } => {
                let name = match variable {
                    Some(name) => name.clone(),
                    None => match interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)? {
                        Value::ValQuote(name) => name,
                        _ => return Err(RuntimeError::ExpectedQuote)
                    }
                };

                let variable = interpreter.variables.get(&name).ok_or_else(|| RuntimeError::UnknownVariable(name.clone())).cloned()?;
                interpreter.stack.push(variable.clone())
            }
            Word::Write { variable, .. } => {
                let name = match variable {
                    Some(name) => name.clone(),
                    None => match interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)? {
                        Value::ValQuote(name) => name,
                        _ => return Err(RuntimeError::ExpectedQuote)
                    }
                };

                let value = interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?;
                interpreter.variables.insert(name, value);
            }
            Word::Execute { variable, .. } => {
                let value = match variable {
                    Some(name) => interpreter.variables.get(name).ok_or_else(|| RuntimeError::UnknownVariable(name.clone())).cloned()?,
                    None => interpreter.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)?
                };

                match value {
                    Value::ValBlock(words) => {
                        for word in words {
                            word.execute(interpreter)?
                        }
                    }
                    _ => return Err(RuntimeError::ExpectedBlock)
                }
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
    UnknownVariable(String),
    ExpectedBlock,
    UnsupportedOperands,
    ExpectedQuote,
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
        Self { words, stack: Vec::new(), variables: HashMap::new() }
    }

    fn run(mut self) -> Result<Vec<Value>, RuntimeError> {
        for word in std::mem::take(&mut self.words) {
            word.execute(&mut self)?
        }

        Ok(self.stack)
    }
}

pub fn interpret(code: &str) -> Result<Vec<Value>, RuntimeError> {
    let words = parse(code)?;
    let interpreter = Interpreter::new(words);
    interpreter.run()
}
