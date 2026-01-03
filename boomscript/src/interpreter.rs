use crate::builtin::{add, and, clear, concat, dip, div, drop, dup, equal, execute, filter, fold, greater, greater_or_equal, iff, keep, less, less_or_equal, map, max, min, mul, nip, not, not_equal, or, over, read, reduce, rem, rot, sub, swap, unless, when, write, xor, Builtin};
use crate::interpreter::RuntimeError::Parse;
use crate::location::{Span, Spanned};
use crate::lowering::lower;
use crate::parser::{parse, ParseError, Word};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::fmt::{Display, Formatter};
use crate::diagnostic::Diagnostic;

pub type RunResult = Result<(), Spanned<RuntimeError>>;

pub trait Executable {
    fn execute(&self, interpreter: &mut Interpreter, span: &Span) -> RunResult;
}

#[derive(Clone, Debug)]
pub enum Value {
    ValInt(i64),
    ValFloat(f64),
    ValChar(char),
    ValString(String),
    ValQuotedWord(String),
    ValBlock(Vec<Spanned<Word>>),
    ValArray(Vec<Value>),
    ValBuiltin(Builtin),
}

impl From<Value> for bool {
    fn from(value: Value) -> Self {
        match value {
            Value::ValInt(0) |
            Value::ValFloat(0.0) => false,
            _ => true
        }
    }
}

impl Executable for Word {
    fn execute(&self, interpreter: &mut Interpreter, span: &Span) -> RunResult {
        match self {
            Word::Int(value) => interpreter.push(Value::ValInt(value.clone())),
            Word::Float(value) => interpreter.push(Value::ValFloat(value.clone())),
            Word::Char(value) => interpreter.push(Value::ValChar(value.clone())),
            Word::String(value) => interpreter.push(Value::ValString(value.clone())),
            Word::QuotedWord(name) => interpreter.push(Value::ValQuotedWord(name.clone())),
            Word::Block(words) => interpreter.push(Value::ValBlock(words.clone())),
            Word::Array(words) => interpreter.push_array(words)?,
            Word::Word(name) => {
                let variable = interpreter.get_variable(name, span)?;
                interpreter.execute(variable, span)?
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
    ExpectedQuotedWord,
    WordHasNegativeStackEffect,
    WordDoesNotHavePositiveStackEffect,
    NonExecutableWord,
    EmptyArray,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::UnsupportedArrayValue => write!(f, "unsupported array value"),
            RuntimeError::EmptyStack => write!(f, "empty stack"),
            RuntimeError::UnknownWord(word) => write!(f, "unknown word: '{word}'"),
            RuntimeError::UnsupportedOperands => write!(f, "unsupported operand"),
            RuntimeError::ExpectedQuotedWord => write!(f, "expected quoted word"),
            RuntimeError::WordHasNegativeStackEffect => write!(f, "word has negative stack effect"),
            RuntimeError::WordDoesNotHavePositiveStackEffect => write!(f, "word does not have positive stack effect"),
            RuntimeError::NonExecutableWord => write!(f, "word cannot be executed"),
            RuntimeError::EmptyArray => write!(f, "empty array"),
        }
    }
}

impl From<ParseError> for RuntimeError {
    fn from(value: ParseError) -> Self {
        Parse(value)
    }
}

// TODO: create frames for execution of blocks to allow for locals
pub struct Interpreter {
    words: VecDeque<Spanned<Word>>,
    pub stack: Vec<Value>,
    pub variables: HashMap<String, Value>,
}

impl Interpreter {
    fn new(words: Vec<Spanned<Word>>) -> Self {
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
                ("reduce".into(), Value::ValBuiltin(Builtin(reduce)))
            ]),
        }
    }

    fn run(mut self) -> Result<Vec<Value>, Vec<Spanned<RuntimeError>>> {
        while let Some(word) = self.words.pop_front() {
            match word.value.execute(&mut self, &word.span) {
                Ok(_) => {}
                Err(error) => return Err(vec![error])
            }
        }

        Ok(self.stack)
    }

    pub fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    pub fn pop(&mut self, span: &Span) -> Result<Value, Spanned<RuntimeError>> {
        self.stack.pop().ok_or_else(|| Spanned::new(RuntimeError::EmptyStack, span.clone()))
    }

    pub fn unary_int_only_op(&mut self, f: impl Fn(i64) -> i64, span: &Span) -> RunResult {
        let top = self.pop(span)?;

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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
        }
    }

    pub fn binary_number_and_char_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64, span: &Span) -> RunResult {
        let top = self.pop(span)?;
        let snd = self.pop(span)?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValInt(f_int(snd_val, top_val)));
                Ok(())
            }
            (Value::ValFloat(snd_val), Value::ValFloat(top_val)) => {
                self.push(Value::ValFloat(f_float(snd_val, top_val)));
                Ok(())
            }
            (Value::ValChar(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValChar(f_int(snd_val as i64, top_val) as u8 as char));
                Ok(())
            }
            (Value::ValChar(snd_val), Value::ValChar(top_val)) => {
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
        }
    }

    pub fn binary_number_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64, span: &Span) -> RunResult {
        let top = self.pop(span)?;
        let snd = self.pop(span)?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValInt(f_int(snd_val, top_val)));
                Ok(())
            }
            (Value::ValFloat(snd_val), Value::ValFloat(top_val)) => {
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => return Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
        }
    }

    pub fn binary_int_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64, span: &Span) -> RunResult {
        let top = self.pop(span)?;
        let snd = self.pop(span)?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => return Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
        }
    }

    pub fn binary_compare_op(&mut self, f_int: impl Fn(&i64, &i64) -> bool, f_float: impl Fn(&f64, &f64) -> bool, span: &Span) -> RunResult {
        let top = self.pop(span)?;
        let snd = self.pop(span)?;

        match (snd, top) {
            (Value::ValInt(snd_val), Value::ValInt(top_val)) => {
                self.push(Value::ValInt(f_int(&snd_val, &top_val).into()));
                Ok(())
            }
            (Value::ValFloat(snd_val), Value::ValFloat(top_val)) => {
                self.push(Value::ValInt(f_float(&snd_val, &top_val).into()));
                Ok(())
            }
            (Value::ValChar(snd_val), Value::ValChar(top_val)) => {
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span.clone()))
                        }
                    }
                }

                self.push(Value::ValArray(array));
                Ok(())
            }
            _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
        }
    }

    pub fn push_array(&mut self, words: &Vec<Spanned<Word>>) -> RunResult {
        let array_start_stack_idx = self.stack.len();

        for Spanned { value: word, span } in words {
            word.execute(self, span)?
        }

        let elements = self.stack.drain(array_start_stack_idx..).collect();
        self.push(Value::ValArray(elements));
        Ok(())
    }

    pub fn get_variable(&mut self, name: &String, span: &Span) -> Result<Value, Spanned<RuntimeError>> {
        match self.variables.get(name) {
            None => Err(Spanned::new(RuntimeError::UnknownWord(name.clone()), span.clone())),
            Some(value) => Ok(value.clone())
        }
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn execute(&mut self, value: Value, span: &Span) -> RunResult {
        match value {
            Value::ValBlock(words) => {
                for Spanned { value: word, span } in words {
                    word.execute(self, &span)?
                }
            }
            Value::ValBuiltin(builtin) => builtin.execute(self, &span)?,
            Value::ValQuotedWord(name) => {
                let value = self.get_variable(&name, &span)?;
                self.execute(value, span)?
            }
            value => self.push(value)
        }

        Ok(())
    }
}

pub fn interpret(code: &str) -> Result<Vec<Value>, Vec<Diagnostic>> {
    let result = match parse(code) {
        Ok(words) => {
            let lowered = lower(words);
            let interpreter = Interpreter::new(lowered);
            interpreter.run()
        }
        Err(errors) => Err(errors.into_iter().map(|error| error.map(RuntimeError::from)).collect())
    };

    result.map_err(|errors| Diagnostic::from_errors(code, &errors))
}
