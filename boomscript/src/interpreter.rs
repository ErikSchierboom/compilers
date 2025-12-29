use crate::parser::{parse, ParseError, Word};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub};

trait Executable {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError>;
}

#[derive(Clone, Debug)]
pub enum Builtin {
    Dup,
    Drop,
    Swap,
    Over,
    Nip,
    When,
    Unless,
    If,
    Clear,
    Rot,
    Dip,
    Keep,
    Map,
    Mod,
    Filter,
    Max,
    Min,
    Fold,
    Reduce,
    Concat,
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
            Builtin::Nip { .. } => {
                let top = interpreter.pop()?;
                interpreter.pop()?;
                interpreter.push(top);
                Ok(())
            }
            Builtin::Clear { .. } => {
                interpreter.stack.clear();
                Ok(())
            }
            Builtin::When => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                if bool::from(snd) {
                    interpreter.execute(top)?
                }

                Ok(())
            }
            Builtin::Unless => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                if !bool::from(snd) {
                    interpreter.execute(top)?
                }

                Ok(())
            }
            Builtin::If => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                let third = interpreter.pop()?;

                if bool::from(third) {
                    interpreter.execute(snd)?
                } else {
                    interpreter.execute(top)?
                }

                Ok(())
            }
            Builtin::Rot => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                let third = interpreter.pop()?;

                interpreter.push(top);
                interpreter.push(third);
                interpreter.push(snd);
                Ok(())
            }
            Builtin::Dip => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                interpreter.execute(top)?;
                interpreter.push(snd);
                Ok(())
            }
            Builtin::Keep => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                interpreter.push(snd.clone());
                interpreter.execute(top)?;
                interpreter.push(snd);

                Ok(())
            }
            Builtin::Mod => interpreter.binary_int_op(i64::rem_euclid),
            Builtin::Max => interpreter.binary_int_op(i64::max),
            Builtin::Min => interpreter.binary_int_op(i64::min),
            Builtin::Map => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                match snd {
                    Value::ValArray(array) => {
                        let mut mapped_array = Vec::new();

                        for element in array {
                            let stack_length_before = interpreter.stack.len();
                            interpreter.push(element);
                            interpreter.execute(top.clone())?;

                            if interpreter.stack.len() < stack_length_before {
                                return Err(RuntimeError::WordHasNegativeStackEffect);
                            }

                            match interpreter.stack.len() - stack_length_before {
                                0 => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect),
                                1 => mapped_array.push(interpreter.pop()?),
                                _ => mapped_array.push(Value::ValArray(interpreter.stack.drain(stack_length_before..).collect())),
                            }
                        }

                        interpreter.push(Value::ValArray(mapped_array))
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }

                Ok(())
            }
            Builtin::Filter => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                match snd {
                    Value::ValArray(array) => {
                        let mut filtered_array = Vec::new();

                        for element in array {
                            let stack_length_before = interpreter.stack.len();
                            interpreter.push(element.clone());
                            interpreter.execute(top.clone())?;

                            match interpreter.stack.len().cmp(&stack_length_before) {
                                Ordering::Less => return Err(RuntimeError::WordHasNegativeStackEffect),
                                Ordering::Equal => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect),
                                Ordering::Greater => {
                                    if interpreter.stack.len() == stack_length_before + 1 {
                                        if bool::from(interpreter.pop()?) {
                                            filtered_array.push(element)
                                        }
                                    }
                                }
                            }
                        }

                        interpreter.push(Value::ValArray(filtered_array))
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }

                Ok(())
            }
            Builtin::Reduce => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                match snd {
                    Value::ValArray(mut array) => {
                        match array.split_first_mut() {
                            Some((head, tail)) => {
                                interpreter.push(head.to_owned());

                                for element in tail.to_owned() {
                                    interpreter.push(element);
                                    interpreter.execute(top.clone())?;
                                }
                            }
                            None => return Err(RuntimeError::EmptyArray),
                        }
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }

                Ok(())
            }
            Builtin::Fold => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;
                let third = interpreter.pop()?;

                match third {
                    Value::ValArray(array) => {
                        interpreter.push(snd);

                        for element in array {
                            interpreter.push(element);
                            interpreter.execute(top.clone())?;
                        }
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }

                Ok(())
            }
            Builtin::Concat => {
                let top = interpreter.pop()?;
                let snd = interpreter.pop()?;

                match (snd, top) {
                    (Value::ValArray(mut l), Value::ValArray(mut r)) => {
                        l.append(&mut r);
                        interpreter.push(Value::ValArray(l));
                    }
                    (Value::ValBlock(mut l), Value::ValBlock(mut r)) => {
                        l.append(&mut r);
                        interpreter.push(Value::ValBlock(l));
                    }
                    (Value::ValString(mut l), Value::ValString(r)) => {
                        l.push_str(&r);
                        interpreter.push(Value::ValString(l));
                    }
                    _ => return Err(RuntimeError::UnsupportedOperands)
                }

                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    ValInt(i64),
    ValChar(char),
    ValString(String),
    ValQuote(String),
    ValBlock(Vec<Word>),
    ValArray(Vec<Value>),
    ValBuiltin(Builtin),
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::ValInt(0) => false,
            _ => true
        }
    }
}

impl Executable for Word {
    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), RuntimeError> {
        match self {
            Word::Int { value, .. } => interpreter.push(Value::ValInt(value.clone())),
            Word::Char { value, .. } => interpreter.push(Value::ValChar(value.clone())),
            Word::String { value, .. } => interpreter.push(Value::ValString(value.clone())),
            Word::Quote { name, .. } => interpreter.push(Value::ValQuote(name.clone())),
            Word::Block { words, .. } => interpreter.push(Value::ValBlock(words.clone())),
            Word::Word { name, .. } => {
                let value = interpreter.get_variable(name)?;
                interpreter.execute(value)?
            }
            Word::Array { words, .. } => interpreter.push_array(words)?,

            Word::Add { .. } => interpreter.binary_int_op(i64::add)?,
            Word::Sub { .. } => interpreter.binary_int_op(i64::sub)?,
            Word::Mul { .. } => interpreter.binary_int_only_op(i64::mul)?,
            Word::Div { .. } => interpreter.binary_int_only_op(i64::div)?,

            Word::And { .. } => interpreter.binary_int_only_op(i64::bitand)?,
            Word::Or { .. } => interpreter.binary_int_only_op(i64::bitor)?,
            Word::Xor { .. } => interpreter.binary_int_only_op(i64::bitxor)?,
            Word::Not { .. } => interpreter.unary_int_op(i64::not)?,

            Word::Greater { .. } => interpreter.binary_compare_op(i64::gt)?,
            Word::GreaterEqual { .. } => interpreter.binary_compare_op(i64::ge)?,
            Word::Less { .. } => interpreter.binary_compare_op(i64::lt)?,
            Word::LessEqual { .. } => interpreter.binary_compare_op(i64::le)?,
            Word::Equal { .. } => interpreter.binary_compare_op(i64::eq)?,
            Word::NotEqual { .. } => interpreter.binary_compare_op(i64::ne)?,

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
                interpreter.set_variable(name, value)
            }
            Word::Execute { .. } => {
                let value = match interpreter.pop()? {
                    Value::ValQuote(name) => interpreter.get_variable(&name)?,
                    Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
                    Value::ValBlock(words) => Value::ValBlock(words),
                    _ => return Err(RuntimeError::ExpectedExecutableWord)
                };
                interpreter.execute(value)?
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    UnsupportedArrayValue,
    EmptyStack,
    UnknownWord(String),
    UnsupportedOperands,
    ExpectedQuote,
    WordHasNegativeStackEffect,
    WordDoesNotHavePositiveStackEffect,
    ExpectedExecutableWord,
    EmptyArray,
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
                ("nip".into(), Value::ValBuiltin(Builtin::Nip)),
                ("when".into(), Value::ValBuiltin(Builtin::When)),
                ("unless".into(), Value::ValBuiltin(Builtin::Unless)),
                ("if".into(), Value::ValBuiltin(Builtin::If)),
                ("clear".into(), Value::ValBuiltin(Builtin::Clear)),
                ("rot".into(), Value::ValBuiltin(Builtin::Rot)),
                ("dip".into(), Value::ValBuiltin(Builtin::Dip)),
                ("keep".into(), Value::ValBuiltin(Builtin::Keep)),
                ("map".into(), Value::ValBuiltin(Builtin::Map)),
                ("filter".into(), Value::ValBuiltin(Builtin::Filter)),
                ("mod".into(), Value::ValBuiltin(Builtin::Mod)),
                ("max".into(), Value::ValBuiltin(Builtin::Max)),
                ("min".into(), Value::ValBuiltin(Builtin::Min)),
                ("fold".into(), Value::ValBuiltin(Builtin::Fold)),
                ("reduce".into(), Value::ValBuiltin(Builtin::Reduce)),
                ("concat".into(), Value::ValBuiltin(Builtin::Concat)),
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

    fn unary_int_op(&mut self, f: impl Fn(i64) -> i64) -> Result<(), RuntimeError> {
        let top = self.pop()?;

        match top {
            Value::ValInt(top_val) => {
                self.push(Value::ValInt(f(top_val)));
                Ok(())
            }
            Value::ValArray(mut array) => {
                let mut mutation_queue = vec![&mut array];

                while let Some(array) = mutation_queue.pop() {
                    for value in array.iter_mut() {
                        match value {
                            Value::ValInt(int) => *int = f(*int),
                            Value::ValArray(inner_values) => {
                                mutation_queue.push(inner_values)
                            }
                            _ => return Err(RuntimeError::UnsupportedArrayValue)
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(RuntimeError::UnsupportedOperands)
        }
    }

    fn binary_int_op(&mut self, f: impl Fn(i64, i64) -> i64) -> Result<(), RuntimeError> {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValInt(f(snd_val, top_val)));
                Ok(())
            }
            (Value::ValChar(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValChar(f(snd_val as i64, top_val) as u8 as char));
                Ok(())
            }
            (Value::ValChar(snd_val), Value::ValChar(top_val)) => {
                self.push(Value::ValInt(f(snd_val as i64, top_val as i64)));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f(*array_int_value, scalar),
                            Value::ValChar(array_char_value) => *array_char_value = f(*array_char_value as i64, scalar) as u8 as char,
                            Value::ValArray(inner_array) => {
                                array_mutation_queue.push(inner_array)
                            }
                            _ => return Err(RuntimeError::UnsupportedArrayValue)
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(RuntimeError::UnsupportedOperands)
        }
    }

    fn binary_int_only_op(&mut self, f: impl Fn(i64, i64) -> i64) -> Result<(), RuntimeError> {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValInt(f(snd_val, top_val)));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f(*array_int_value, scalar),
                            Value::ValArray(inner_array) => {
                                array_mutation_queue.push(inner_array)
                            }
                            _ => return Err(RuntimeError::UnsupportedArrayValue)
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(RuntimeError::UnsupportedOperands)
        }
    }

    fn binary_compare_op(&mut self, f: impl Fn(&i64, &i64) -> bool) -> Result<(), RuntimeError> {
        self.binary_int_op(|l, r| f(&l, &r) as i64)
    }

    fn push_array(&mut self, words: &Vec<Word>) -> Result<(), RuntimeError> {
        let stack_size_before = self.stack.len();

        for word in words {
            word.execute(self)?
        }

        if self.stack.len() < stack_size_before {
            return Err(RuntimeError::WordHasNegativeStackEffect);
        }

        let elements = self.stack.drain(stack_size_before..).collect();
        self.push(Value::ValArray(elements));
        Ok(())
    }

    fn get_variable(&mut self, name: &String) -> Result<Value, RuntimeError> {
        self.variables.get(name).ok_or_else(|| RuntimeError::UnknownWord(name.clone())).cloned()
    }

    fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn execute(&mut self, value: Value) -> Result<(), RuntimeError> {
        match value {
            Value::ValBlock(words) => {
                for word in words {
                    word.execute(self)?
                }
            }
            Value::ValBuiltin(builtin) => builtin.execute(self)?,
            Value::ValQuote(name) => {
                let value = self.get_variable(&name)?;
                self.execute(value)?
            }
            value => self.push(value)
        }

        Ok(())
    }
}

pub fn interpret(code: &str) -> Result<Vec<Value>, RuntimeError> {
    let words = parse(code)?;
    let interpreter = Interpreter::new(words);
    interpreter.run()
}
