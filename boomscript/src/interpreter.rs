use crate::interpreter::RuntimeError::Parse;
use crate::location::Span;
use crate::lowering::lower;
use crate::parser::{parse, ParseError, Word};
use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
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

fn add(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_and_char_op(i64::add, f64::add) }
fn sub(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_and_char_op(i64::sub, f64::sub) }
fn mul(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_only_op(i64::mul, f64::mul) }
fn div(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_only_op(i64::div, f64::mul) }
fn and(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitand) }
fn or(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitor) }
fn xor(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_int_only_op(i64::bitxor) }
fn not(interpreter: &mut Interpreter) -> RunResult { interpreter.unary_int_only_op(i64::not) }
fn greater(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::gt, f64::gt) }
fn greater_or_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::ge, f64::ge) }
fn less(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::lt, f64::lt) }
fn less_or_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::le, f64::le) }
fn equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::eq, f64::eq) }
fn not_equal(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_compare_op(i64::ne, f64::ne) }
fn rem(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_and_char_op(i64::rem_euclid, f64::rem_euclid) }
fn max(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_and_char_op(i64::max, f64::max) }
fn min(interpreter: &mut Interpreter) -> RunResult { interpreter.binary_number_and_char_op(i64::min, f64::min) }

fn read(interpreter: &mut Interpreter) -> RunResult {
    let (name, location) = match interpreter.pop()? {
        Value::ValQuote(name, location) => (name, location),
        value => return Err(RuntimeError::ExpectedQuote(value.location().clone()))
    };
    let variable = interpreter.get_variable(&name, &location)?;
    interpreter.push(variable.clone());
    Ok(())
}

fn write(interpreter: &mut Interpreter) -> RunResult {
    let name = match interpreter.pop()? {
        Value::ValQuote(name, _) => name,
        value => return Err(RuntimeError::ExpectedQuote(value.location().clone()))
    };
    let value = interpreter.pop()?;
    interpreter.set_variable(name, value);
    Ok(())
}

fn execute(interpreter: &mut Interpreter) -> RunResult {
    let value = match interpreter.pop()? {
        Value::ValQuote(name, location) => interpreter.get_variable(&name, &location)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words, location) => Value::ValBlock(words, location),
        value => return Err(RuntimeError::ExpectedExecutableWord(value.location().clone()))
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
        Value::ValArray(array, location) => {
            let mut mapped_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element);
                interpreter.execute(top.clone())?;

                if interpreter.stack.len() < stack_length_before {
                    return Err(RuntimeError::WordHasNegativeStackEffect(location.clone()));
                }

                match interpreter.stack.len() - stack_length_before {
                    0 => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect(location.clone())),
                    1 => mapped_array.push(interpreter.pop()?),
                    _ => mapped_array.push(Value::ValArray(interpreter.stack.drain(stack_length_before..).collect(), location)),
                }
            }

            interpreter.push(Value::ValArray(mapped_array, location))
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

fn filter(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;

    match snd {
        Value::ValArray(array, location) => {
            let mut filtered_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element.clone());
                interpreter.execute(top.clone())?;

                match interpreter.stack.len().cmp(&stack_length_before) {
                    Ordering::Less => return Err(RuntimeError::WordHasNegativeStackEffect(location)),
                    Ordering::Equal => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect(location)),
                    Ordering::Greater => {
                        if interpreter.stack.len() == stack_length_before + 1 {
                            if bool::from(interpreter.pop()?) {
                                filtered_array.push(element)
                            }
                        }
                    }
                }
            }

            interpreter.push(Value::ValArray(filtered_array, location))
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

fn reduce(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;

    match snd {
        Value::ValArray(mut array, location) => {
            match array.split_first_mut() {
                Some((head, tail)) => {
                    interpreter.push(head.to_owned());

                    for element in tail.to_owned() {
                        interpreter.push(element);
                        interpreter.execute(top.clone())?;
                    }
                }
                None => return Err(RuntimeError::EmptyArray(location)),
            }
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

fn fold(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;
    let third = interpreter.pop()?;

    match third {
        Value::ValArray(array, _) => {
            interpreter.push(snd);

            for element in array {
                interpreter.push(element);
                interpreter.execute(top.clone())?;
            }
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

fn concat(interpreter: &mut Interpreter) -> RunResult {
    let top = interpreter.pop()?;
    let snd = interpreter.pop()?;

    match (snd, top) {
        (Value::ValArray(mut l, location_l), Value::ValArray(mut r, location_r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValArray(l, location_l.merge(&location_r)));
        }
        (Value::ValBlock(mut l, location_l), Value::ValBlock(mut r, location_r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValBlock(l, location_l.merge(&location_r)));
        }
        (Value::ValString(mut l, location_l), Value::ValString(r, location_r)) => {
            l.push_str(&r);
            interpreter.push(Value::ValString(l, location_l.merge(&location_r)));
        }
        (l, r) => return Err(RuntimeError::UnsupportedOperands(l.location().merge(r.location())))
    }

    Ok(())
}

#[derive(Clone, Debug)]
pub enum Value {
    ValInt(i64, Span),
    ValFloat(f64, Span),
    ValChar(char, Span),
    ValString(String, Span),
    ValQuote(String, Span),
    ValBlock(Vec<Word>, Span),
    ValArray(Vec<Value>, Span),
    ValBuiltin(Builtin), // TODO: check if this needs a span too
}

impl Value {
    fn location(&self) -> &Span {
        match self {
            Value::ValInt(_, location) |
            Value::ValFloat(_, location) |
            Value::ValChar(_, location) |
            Value::ValString(_, location) |
            Value::ValQuote(_, location) |
            Value::ValBlock(_, location) |
            Value::ValArray(_, location) => location,
            Value::ValBuiltin(_) => &Span::EMPTY
        }
    }
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::ValInt(0, _) => false,
            Value::ValFloat(0.0, _) => false,
            _ => true
        }
    }
}

impl Executable for Word {
    fn execute(&self, interpreter: &mut Interpreter) -> RunResult {
        // TODO: get rid of clones
        match self {
            Word::Int(value, location) => interpreter.push(Value::ValInt(value.clone(), location.clone())),
            Word::Float(value, location) => interpreter.push(Value::ValFloat(value.clone(), location.clone())),
            Word::Char(value, location) => interpreter.push(Value::ValChar(value.clone(), location.clone())),
            Word::String(value, location) => interpreter.push(Value::ValString(value.clone(), location.clone())),
            Word::Quote(name, location) => interpreter.push(Value::ValQuote(name.clone(), location.clone())),
            Word::Block(words, location) => interpreter.push(Value::ValBlock(words.clone(), location.clone())),
            Word::Array(words, location) => interpreter.push_array(words, location)?,
            Word::Name(name, location) => {
                let value = interpreter.get_variable(name, location)?;
                interpreter.execute(value)?
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError, Span),
    UnsupportedArrayValue(Span),
    EmptyStack(Span),
    UnknownWord(String, Span),
    UnsupportedOperands(Span),
    ExpectedQuote(Span),
    WordHasNegativeStackEffect(Span),
    WordDoesNotHavePositiveStackEffect(Span),
    ExpectedExecutableWord(Span),
    EmptyArray(Span),
}

impl From<ParseError> for RuntimeError {
    fn from(value: ParseError) -> Self {
        let location = value.location().clone();
        Parse(value, location)
    }
}

struct Interpreter {
    words: VecDeque<Word>,
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
}

impl Interpreter {
    fn new(words: Vec<Word>) -> Self {
        Self {
            words: words.into_iter().collect(),
            stack: Vec::new(),
            variables: HashMap::from([
                // TODO: support ? for printing stack and . for pop and print top
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

    fn run(mut self) -> Result<Vec<Value>, Vec<RuntimeError>> {
        while let Some(word) = self.words.pop_front() {
            match word.execute(&mut self) {
                Ok(_) => {}
                Err(error) => return Err(vec![error])
            }
        }

        Ok(self.stack)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    fn unary_int_only_op(&mut self, f: impl Fn(i64) -> i64) -> RunResult {
        let top = self.pop()?;

        match top {
            Value::ValInt(top_val, location) => {
                self.push(Value::ValInt(f(top_val), location));
                Ok(())
            }
            Value::ValArray(mut array, location) => {
                let mut mutation_queue = vec![&mut array];

                while let Some(array) = mutation_queue.pop() {
                    for value in array.iter_mut() {
                        match value {
                            Value::ValInt(int, _) => *int = f(*int),
                            Value::ValArray(inner_values, _) => {
                                mutation_queue.push(inner_values)
                            }
                            _ => return Err(RuntimeError::UnsupportedArrayValue(location))
                        }
                    }
                }

                self.push(Value::ValArray(array, location));
                Ok(())
            }
            value => Err(RuntimeError::UnsupportedOperands(value.location().clone()))
        }
    }

    fn binary_number_and_char_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val, snd_location), Value::ValInt(top_val, top_location)) => {
                self.push(Value::ValInt(f_int(snd_val, top_val), snd_location.merge(&top_location)));
                Ok(())
            }
            (Value::ValFloat(snd_val, snd_loc), Value::ValFloat(top_val, top_loc)) => {
                self.push(Value::ValFloat(f_float(snd_val, top_val)));
                Ok(())
            }
            (Value::ValChar(snd_val, snd_loc), Value::ValInt(top_val, top_loc)) => {
                self.push(Value::ValChar(f_int(snd_val as i64, top_val) as u8 as char));
                Ok(())
            }
            (Value::ValChar(snd_val, snd_loc), Value::ValChar(top_val, top_loc)) => {
                self.push(Value::ValInt(f_int(snd_val as i64, top_val as i64)));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f_int(*array_int_value, scalar),
                            Value::ValChar(array_char_value) => *array_char_value = f_int(*array_char_value as i64, scalar) as u8 as char,
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
            (Value::ValFloat(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValFloat(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValFloat(array_int_value) => *array_int_value = f_float(*array_int_value, scalar),
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

    fn binary_number_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val, snd_loc), Value::ValInt(top_val, top_loc)) => {
                self.push(Value::ValInt(f_int(snd_val, top_val)));
                Ok(())
            }
            (Value::ValFloat(snd_val, snd_loc), Value::ValFloat(top_val, top_loc)) => {
                self.push(Value::ValFloat(f_float(snd_val, top_val)));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f_int(*array_int_value, scalar),
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
            (Value::ValFloat(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValFloat(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValFloat(array_int_value) => *array_int_value = f_float(*array_int_value, scalar),
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

    fn binary_int_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val, snd_loc), Value::ValInt(top_val, top_loc)) => {
                self.push(Value::ValInt(f_int(snd_val, top_val)));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f_int(*array_int_value, scalar),
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

    fn binary_compare_op(&mut self, f_int: impl Fn(&i64, &i64) -> bool, f_float: impl Fn(&f64, &f64) -> bool) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

        match (snd, top) {
            (Value::ValInt(snd_val, snd_loc), Value::ValInt(top_val, top_loc)) => {
                self.push(Value::ValInt(f_int(&snd_val, &top_val).into()));
                Ok(())
            }
            (Value::ValFloat(snd_val, snd_loc), Value::ValFloat(top_val, top_loc)) => {
                self.push(Value::ValInt(f_float(&snd_val, &top_val).into()));
                Ok(())
            }
            (Value::ValChar(snd_val, snd_loc), Value::ValChar(top_val, top_loc)) => {
                self.push(Value::ValInt(f_int(&(snd_val as i64), &(top_val as i64)).into()));
                Ok(())
            }
            (Value::ValInt(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValInt(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValInt(array_int_value) => *array_int_value = f_int(array_int_value, &scalar).into(),
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
            (Value::ValFloat(scalar), Value::ValArray(mut array)) |
            (Value::ValArray(mut array), Value::ValFloat(scalar)) => {
                let mut array_mutation_queue = vec![&mut array];

                while let Some(array_to_mutate) = array_mutation_queue.pop() {
                    for array_val_to_mutate in array_to_mutate.iter_mut() {
                        match array_val_to_mutate {
                            Value::ValFloat(array_float_value) => *array_val_to_mutate = Value::ValInt(f_float(array_float_value, &scalar).into()),
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
            (snd, top) => Err(RuntimeError::UnsupportedOperands(snd.location().merge(top.location())))
        }
    }

    fn push_array(&mut self, words: &Vec<Word>, location: &Span) -> RunResult {
        let array_start_stack_idx = self.stack.len();

        for word in words {
            word.execute(self)?
        }

        let elements = self.stack.drain(array_start_stack_idx..).collect();
        self.push(Value::ValArray(elements, location.clone()));
        Ok(())
    }

    fn get_variable(&mut self, name: &String, location: &Span) -> Result<Value, RuntimeError> {
        match self.variables.get(name) {
            None => Err(RuntimeError::UnknownWord(name.clone(), location.clone())),
            Some(value) => Ok(value.clone())
        }
    }

    fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn execute(&mut self, value: Value) -> RunResult {
        match value {
            Value::ValBlock(words, _) => {
                for word in words {
                    word.execute(self)?
                }
            }
            Value::ValBuiltin(builtin) => builtin.execute(self)?,
            Value::ValQuote(name, location) => {
                let value = self.get_variable(&name, &location)?;
                self.execute(value)?
            }
            value => self.push(value)
        }

        Ok(())
    }
}

pub fn interpret(code: &str) -> Result<Vec<Value>, Vec<RuntimeError>> {
    match parse(code) {
        Ok(words) => {
            let lowered = lower(words);
            let interpreter = Interpreter::new(lowered);
            interpreter.run()
        }
        Err(errors) => Err(errors.into_iter().map(RuntimeError::from).collect())
    }
}
