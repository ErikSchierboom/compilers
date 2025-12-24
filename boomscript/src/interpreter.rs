use crate::parser::{parse, BuiltinKind, ParseError, Word};
use std::collections::HashMap;

trait Executable {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError>;
}

impl Executable for BuiltinKind {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            BuiltinKind::Dup { .. } => {
                let last = interpreter.stack.last().unwrap_or_else(|| panic!("not enough values on stack"));
                interpreter.stack.push(last.clone());
                Ok(())
            }
            BuiltinKind::Drop { .. } => {
                interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                Ok(())
            }
            BuiltinKind::Swap { .. } => {
                let r = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                let l = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                interpreter.stack.push(r);
                interpreter.stack.push(l);
                Ok(())
            }
            BuiltinKind::Over { .. } => {
                let r = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                let l = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
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
            Word::Builtin { kind, .. } => kind.execute(interpreter)?,
            Word::Block { words, .. } => interpreter.stack.push(Value::ValBlock(words.clone())),
            Word::Array { words: elements, .. } => {
                let stack_size_before = interpreter.stack.len();

                for element in elements {
                    element.execute(interpreter)?
                }

                let elements = interpreter.stack.drain(stack_size_before..).collect();
                interpreter.stack.push(Value::ValArray(elements))
            }
            Word::Add { .. } => {
                let r = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                let l = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
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
                    _ => panic!("cannot add values on stack")
                }
            }
            Word::Mul { .. } => {
                let r = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                let l = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                match (l, r) {
                    (Value::ValInt(l_i), Value::ValInt(r_i)) => interpreter.stack.push(Value::ValInt(l_i * r_i)),
                    _ => panic!("cannot add values on stack")
                }
            }
            Word::Read { variable, .. } => {
                let name = match variable {
                    Some(name) => name.clone(),
                    None => match interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack")) {
                        Value::ValQuote(name) => name,
                        _ => panic!("expected quoted string")
                    }
                };

                let variable = interpreter.variables.get(&name).unwrap_or_else(|| panic!("could not find variable"));
                interpreter.stack.push(variable.clone())
            }
            Word::Write { variable, .. } => {
                let name = match variable {
                    Some(name) => name.clone(),
                    None => match interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack")) {
                        Value::ValQuote(name) => name,
                        _ => panic!("expected quoted string")
                    }
                };

                let value = interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"));
                interpreter.variables.insert(name, value);
            }
            Word::Execute { variable, .. } => {
                let value = match variable {
                    Some(name) => interpreter.variables.get(name).unwrap_or_else(|| panic!("could not find variable")).clone(),
                    None => interpreter.stack.pop().unwrap_or_else(|| panic!("not enough values on stack"))
                };

                match value {
                    Value::ValBlock(words) => {
                        for word in words {
                            word.execute(interpreter)?
                        }
                    }
                    _ => panic!("expected quoted string")
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
        let words = self.words.clone();
        for word in words {
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
