use crate::lowering::lower;
use crate::parser::{parse, ParseError, Word};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub};

type RunResult = Result<(), RuntimeError>;

trait Executable {
    fn execute(&self, interpreter: &mut Interpreter) -> RunResult;
}

#[derive(Clone, Debug)]
pub struct Builtin(fn(&mut Interpreter) -> RunResult);

impl Executable for Builtin {
    fn execute(&self, interpreter: &mut Interpreter) -> RunResult {
        self.0(interpreter)
    }
}

fn add(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_op(i64::add) }
fn sub(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_op(i64::sub) }
fn mul(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::mul) }
fn div(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::div) }
fn and(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitand) }
fn or(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitor) }
fn xor(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitxor) }
fn not(interpreter: &mut Interpreter) -> RunResult { interpreter.unary_int_op(i64::not) }
fn greater(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::gt) }
fn greater_or_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::ge) }
fn less(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::lt) }
fn less_or_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::le) }
fn equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::eq) }
fn not_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::ne) }
fn rem(interpreter: &mut Interpreter) -> RunResult {
    interpreter.binary_int_op(i64::rem_euclid)
}
fn max(interpreter: &mut Interpreter) -> RunResult {
    interpreter.binary_int_op(i64::max)
}
fn min(interpreter: &mut Interpreter) -> RunResult {
    interpreter.binary_int_op(i64::min)
}

fn read(interpreter: &mut Interpreter) -> RunResult {
    let name = match interpreter.pop()? {
        Value::ValQuote(name) => name,
        _ => return Err(RuntimeError::ExpectedQuote)
    };
    let variable = interpreter.get_variable(&name)?;
    interpreter.push(variable.clone());
    Ok(())
}

fn write(interpreter: &mut Interpreter) -> RunResult {
    let name = match interpreter.pop()? {
        Value::ValQuote(name) => name,
        _ => return Err(RuntimeError::ExpectedQuote)
    };
    let value = interpreter.pop()?;
    interpreter.set_variable(name, value);
    Ok(())
}

fn execute(interpreter: &mut Interpreter) -> RunResult {
    let value = match interpreter.pop()? {
        Value::ValQuote(name) => interpreter.get_variable(&name)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words) => Value::ValBlock(words),
        _ => return Err(RuntimeError::ExpectedExecutableWord)
    };
    interpreter.execute(value)
}

fn dup(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    interpreter.push(top.clone());
    interpreter.push(top);
    Ok(())
}

fn drop(interpreter: &mut Interpreter) -> RunResult {
    interpreter.pop()?;
    Ok(())
}

fn swap(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

fn over(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    interpreter.push(snd.clone());
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

fn nip(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    interpreter.pop()?;
    interpreter.push(top);
    Ok(())
}

fn clear(interpreter: &mut Interpreter) -> RunResult {
    interpreter.stack.clear();
    Ok(())
}

fn when(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;

    if bool::from(snd) {
        interpreter.execute(top)?
    }

    Ok(())
}

fn unless(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;

    if !bool::from(snd) {
        interpreter.execute(top)?
    }

    Ok(())
}

fn iff(interpreter: &mut Interpreter) -> RunResult {
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

fn rot(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    let third = interpreter.pop()?;

    interpreter.push(top);
    interpreter.push(third);
    interpreter.push(snd);
    Ok(())
}

fn dip(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    interpreter.execute(top)?;
    interpreter.push(snd);
    Ok(())
}

fn keep(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    interpreter.push(snd.clone());
    interpreter.execute(top)?;
    interpreter.push(snd);
    Ok(())
}

fn map(interpreter: &mut Interpreter) -> RunResult {
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

fn filter(interpreter: &mut Interpreter) -> RunResult {
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

fn reduce(interpreter: &mut Interpreter) -> RunResult {
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

fn fold(interpreter: &mut Interpreter) -> RunResult {
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

fn concat(interpreter: &mut Interpreter) -> RunResult {
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
    fn execute(&self, interpreter: &mut Interpreter) -> RunResult {
        match self {
            Word::Int { value, .. } => interpreter.push(Value::ValInt(value.clone())),
            Word::Char { value, .. } => interpreter.push(Value::ValChar(value.clone())),
            Word::String { value, .. } => interpreter.push(Value::ValString(value.clone())),
            Word::Quote { name, .. } => interpreter.push(Value::ValQuote(name.clone())),
            Word::Block { words, .. } => interpreter.push(Value::ValBlock(words.clone())),
            Word::Array { words, .. } => interpreter.push_array(words)?,
            Word::Name { name, .. } => {
                let value = interpreter.get_variable(name)?;
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
                ("+".into(), Value::ValBuiltin(Builtin(add))),
                ("-".into(), Value::ValBuiltin(Builtin(sub))),
                ("*".into(), Value::ValBuiltin(Builtin(mul))),
                ("/".into(), Value::ValBuiltin(Builtin(div))),
                ("&".into(), Value::ValBuiltin(Builtin(and))),
                ("|".into(), Value::ValBuiltin(Builtin(or))),
                ("^".into(), Value::ValBuiltin(Builtin(xor))),
                ("!".into(), Value::ValBuiltin(Builtin(not))),
                (">".into(), Value::ValBuiltin(Builtin(greater))),
                (">=".into(), Value::ValBuiltin(Builtin(greater_or_equal))),
                ("<".into(), Value::ValBuiltin(Builtin(less))),
                ("<=".into(), Value::ValBuiltin(Builtin(less_or_equal))),
                ("=".into(), Value::ValBuiltin(Builtin(equal))),
                ("!=".into(), Value::ValBuiltin(Builtin(not_equal))),
                ("++".into(), Value::ValBuiltin(Builtin(concat))),
                ("@".into(), Value::ValBuiltin(Builtin(read))),
                ("$".into(), Value::ValBuiltin(Builtin(write))),
                ("%".into(), Value::ValBuiltin(Builtin(execute))),
                ("dup".into(), Value::ValBuiltin(Builtin(dup))),
                ("drop".into(), Value::ValBuiltin(Builtin(drop))),
                ("swap".into(), Value::ValBuiltin(Builtin(swap))),
                ("over".into(), Value::ValBuiltin(Builtin(over))),
                ("nip".into(), Value::ValBuiltin(Builtin(nip))),
                ("when".into(), Value::ValBuiltin(Builtin(when))),
                ("unless".into(), Value::ValBuiltin(Builtin(unless))),
                ("if".into(), Value::ValBuiltin(Builtin(iff))),
                ("clear".into(), Value::ValBuiltin(Builtin(clear))),
                ("rot".into(), Value::ValBuiltin(Builtin(rot))),
                ("dip".into(), Value::ValBuiltin(Builtin(dip))),
                ("keep".into(), Value::ValBuiltin(Builtin(keep))),
                ("map".into(), Value::ValBuiltin(Builtin(map))),
                ("filter".into(), Value::ValBuiltin(Builtin(filter))),
                ("mod".into(), Value::ValBuiltin(Builtin(rem))),
                ("max".into(), Value::ValBuiltin(Builtin(max))),
                ("min".into(), Value::ValBuiltin(Builtin(min))),
                ("fold".into(), Value::ValBuiltin(Builtin(fold))),
                ("reduce".into(), Value::ValBuiltin(Builtin(reduce))),
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

    fn unary_int_op(&mut self, f: impl Fn(i64) -> i64) -> RunResult {
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

    fn binary_int_op(&mut self, f: impl Fn(i64, i64) -> i64) -> RunResult {
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

    fn binary_int_only_op(&mut self, f: impl Fn(i64, i64) -> i64) -> RunResult {
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

    fn binary_compare_op(&mut self, f: impl Fn(&i64, &i64) -> bool) -> RunResult {
        self.binary_int_op(|l, r| f(&l, &r) as i64)
    }

    fn push_array(&mut self, words: &Vec<Word>) -> RunResult {
        let array_start_stack_idx = self.stack.len();

        for word in words {
            word.execute(self)?
        }

        let elements = self.stack.drain(array_start_stack_idx..).collect();
        self.push(Value::ValArray(elements));
        Ok(())
    }

    fn get_variable(&mut self, name: &String) -> Result<Value, RuntimeError> {
        self.variables.get(name).ok_or_else(|| RuntimeError::UnknownWord(name.clone())).cloned()
    }

    fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn execute(&mut self, value: Value) -> RunResult {
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
    let words = lower(parse(code)?);
    let interpreter = Interpreter::new(words);
    interpreter.run()
}
