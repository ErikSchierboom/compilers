use crate::builtin::{add, and, clear, concat, dip, div, drop, dup, equal, execute, filter, fold, greater, greater_or_equal, iff, keep, less, less_or_equal, map, max, min, mul, nip, not, not_equal, or, over, read, reduce, rem, rot, sub, swap, unless, when, write, xor, Builtin};
use crate::interpreter::RuntimeError::Parse;
use crate::location::{Span, Spanned};
use crate::lowering::lower;
use crate::parser::{parse, ParseError, Word};
use std::collections::{HashMap, VecDeque};

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
    ValArray(Vec<Spanned<Value>>),
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
            Word::Int(value) => interpreter.push(Spanned::new(Value::ValInt(value.clone()), span.clone())),
            Word::Float(value) => interpreter.push(Spanned::new(Value::ValFloat(value.clone()), span.clone())),
            Word::Char(value) => interpreter.push(Spanned::new(Value::ValChar(value.clone()), span.clone())),
            Word::String(value) => interpreter.push(Spanned::new(Value::ValString(value.clone()), span.clone())),
            Word::QuotedWord(name) => interpreter.push(Spanned::new(Value::ValQuotedWord(name.clone()), span.clone())),
            Word::Block(words) => interpreter.push(Spanned::new(Value::ValBlock(words.clone()), span.clone())),
            Word::Array(words) => interpreter.push_array(words, span)?,
            Word::Word(name) => {
                let variable = interpreter.get_variable(name, span)?;
                interpreter.execute(variable)?
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
        Parse(value)
    }
}

pub struct Interpreter {
    words: VecDeque<Spanned<Word>>,
    pub stack: Vec<Spanned<Value>>,
    pub variables: HashMap<String, Spanned<Value>>,
}

impl Interpreter {
    fn new(words: Vec<Spanned<Word>>) -> Self {
        Self {
            words: words.into_iter().collect(),
            stack: Vec::new(),
            variables: HashMap::from([
                // TODO: support ? for printing stack and . for pop and print top
                ("+".into(), Spanned::new(Value::ValBuiltin(Builtin(add)), Span::EMPTY)),
                ("-".into(), Spanned::new(Value::ValBuiltin(Builtin(sub)), Span::EMPTY)),
                ("*".into(), Spanned::new(Value::ValBuiltin(Builtin(mul)), Span::EMPTY)),
                ("/".into(), Spanned::new(Value::ValBuiltin(Builtin(div)), Span::EMPTY)),
                ("&".into(), Spanned::new(Value::ValBuiltin(Builtin(and)), Span::EMPTY)),
                ("|".into(), Spanned::new(Value::ValBuiltin(Builtin(or)), Span::EMPTY)),
                ("^".into(), Spanned::new(Value::ValBuiltin(Builtin(xor)), Span::EMPTY)),
                ("!".into(), Spanned::new(Value::ValBuiltin(Builtin(not)), Span::EMPTY)),
                (">".into(), Spanned::new(Value::ValBuiltin(Builtin(greater)), Span::EMPTY)),
                (">=".into(), Spanned::new(Value::ValBuiltin(Builtin(greater_or_equal)), Span::EMPTY)),
                ("<".into(), Spanned::new(Value::ValBuiltin(Builtin(less)), Span::EMPTY)),
                ("<=".into(), Spanned::new(Value::ValBuiltin(Builtin(less_or_equal)), Span::EMPTY)),
                ("=".into(), Spanned::new(Value::ValBuiltin(Builtin(equal)), Span::EMPTY)),
                ("!=".into(), Spanned::new(Value::ValBuiltin(Builtin(not_equal)), Span::EMPTY)),
                ("++".into(), Spanned::new(Value::ValBuiltin(Builtin(concat)), Span::EMPTY)),
                ("@".into(), Spanned::new(Value::ValBuiltin(Builtin(read)), Span::EMPTY)),
                ("$".into(), Spanned::new(Value::ValBuiltin(Builtin(write)), Span::EMPTY)),
                ("%".into(), Spanned::new(Value::ValBuiltin(Builtin(execute)), Span::EMPTY)),
                ("dup".into(), Spanned::new(Value::ValBuiltin(Builtin(dup)), Span::EMPTY)),
                ("drop".into(), Spanned::new(Value::ValBuiltin(Builtin(drop)), Span::EMPTY)),
                ("swap".into(), Spanned::new(Value::ValBuiltin(Builtin(swap)), Span::EMPTY)),
                ("over".into(), Spanned::new(Value::ValBuiltin(Builtin(over)), Span::EMPTY)),
                ("nip".into(), Spanned::new(Value::ValBuiltin(Builtin(nip)), Span::EMPTY)),
                ("when".into(), Spanned::new(Value::ValBuiltin(Builtin(when)), Span::EMPTY)),
                ("unless".into(), Spanned::new(Value::ValBuiltin(Builtin(unless)), Span::EMPTY)),
                ("if".into(), Spanned::new(Value::ValBuiltin(Builtin(iff)), Span::EMPTY)),
                ("clear".into(), Spanned::new(Value::ValBuiltin(Builtin(clear)), Span::EMPTY)),
                ("rot".into(), Spanned::new(Value::ValBuiltin(Builtin(rot)), Span::EMPTY)),
                ("dip".into(), Spanned::new(Value::ValBuiltin(Builtin(dip)), Span::EMPTY)),
                ("keep".into(), Spanned::new(Value::ValBuiltin(Builtin(keep)), Span::EMPTY)),
                ("map".into(), Spanned::new(Value::ValBuiltin(Builtin(map)), Span::EMPTY)),
                ("filter".into(), Spanned::new(Value::ValBuiltin(Builtin(filter)), Span::EMPTY)),
                ("mod".into(), Spanned::new(Value::ValBuiltin(Builtin(rem)), Span::EMPTY)),
                ("max".into(), Spanned::new(Value::ValBuiltin(Builtin(max)), Span::EMPTY)),
                ("min".into(), Spanned::new(Value::ValBuiltin(Builtin(min)), Span::EMPTY)),
                ("fold".into(), Spanned::new(Value::ValBuiltin(Builtin(fold)), Span::EMPTY)),
                ("reduce".into(), Spanned::new(Value::ValBuiltin(Builtin(reduce)), Span::EMPTY))
            ]),
        }
    }

    fn run(mut self) -> Result<Vec<Spanned<Value>>, Vec<Spanned<RuntimeError>>> {
        while let Some(word) = self.words.pop_front() {
            match word.value.execute(&mut self, &word.span) {
                Ok(_) => {}
                Err(error) => return Err(vec![error])
            }
        }

        Ok(self.stack)
    }

    pub fn push(&mut self, value: Spanned<Value>) {
        self.stack.push(value)
    }

    pub fn pop(&mut self, span: &Span) -> Result<Spanned<Value>, Spanned<RuntimeError>> {
        self.stack.pop().ok_or_else(|| Spanned::new(RuntimeError::EmptyStack, span.clone()))
    }

    pub fn unary_int_only_op(&mut self, f: impl Fn(i64) -> i64, span: &Span) -> RunResult {
        let Spanned { value: top, span } = self.pop()?;

        match top {
            Value::ValInt(top_val) => {
                self.push(Spanned::new(Value::ValInt(f(top_val)), span));
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
                            _ => return Err(Spanned::new(RuntimeError::UnsupportedArrayValue, span))
                        }
                    }
                }

                self.push(Spanned::new(Value::ValArray(array), span));
                Ok(())
            }
            _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span))
        }
    }

    pub fn binary_number_and_char_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64, span: &Span) -> RunResult {
        let Spanned { value: top, span: top_span } = self.pop()?;
        let Spanned { value: snd, span: snd_span } = self.pop()?;

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

    pub fn binary_number_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64, f_float: impl Fn(f64, f64) -> f64, span: &Span) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

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

    pub fn binary_int_only_op(&mut self, f_int: impl Fn(i64, i64) -> i64, span: &Span) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

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

    pub fn binary_compare_op(&mut self, f_int: impl Fn(&i64, &i64) -> bool, f_float: impl Fn(&f64, &f64) -> bool, span: &Span) -> RunResult {
        let top = self.pop()?;
        let snd = self.pop()?;

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

    pub fn push_array(&mut self, words: &Vec<Spanned<Word>>, span: &Span) -> RunResult {
        let array_start_stack_idx = self.stack.len();

        for Spanned { value: word, span } in words {
            word.execute(self, span)?
        }

        let elements = self.stack.drain(array_start_stack_idx..).collect();
        self.push(Spanned::new(Value::ValArray(elements), span.clone()));
        Ok(())
    }

    pub fn get_variable(&mut self, name: &String, span: &Span) -> Result<Spanned<Value>, Spanned<RuntimeError>> {
        match self.variables.get(name) {
            None => Err(Spanned::new(RuntimeError::UnknownWord(name.clone()), span.clone())),
            Some(value) => Ok(value.clone())
        }
    }

    pub fn set_variable(&mut self, name: String, value: Value, span: Span) {
        self.variables.insert(name, Spanned::new(value, span));
    }

    pub fn execute(&mut self, spanned_value: Spanned<Value>) -> RunResult {
        let Spanned { value, span } = spanned_value;
        match value {
            Value::ValBlock(words) => {
                for Spanned { value: word, span } in words {
                    word.execute(self, &span)?
                }
            }
            Value::ValBuiltin(builtin) => builtin.execute(self, &span)?,
            Value::ValQuotedWord(name) => {
                let value = self.get_variable(&name, &span)?;
                self.execute(value)?
            }
            value => self.push(Spanned { value, span })
        }

        Ok(())
    }
}

pub fn interpret(code: &str) -> Result<Vec<Spanned<Value>>, Vec<Spanned<RuntimeError>>> {
    match parse(code) {
        Ok(words) => {
            let lowered = lower(words);
            let interpreter = Interpreter::new(lowered);
            interpreter.run()
        }
        Err(errors) => Err(errors.into_iter().map(|error| error.map(RuntimeError::from)).collect())
    }
}
