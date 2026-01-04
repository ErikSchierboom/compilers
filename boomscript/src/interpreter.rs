use crate::builtin::Builtin;
use crate::diagnostic::Diagnostic;
use crate::interpreter::RuntimeError::Parse;
use crate::location::{Span, Spanned};
use crate::lowering::lower;
use crate::parser::{parse, ParseError, Word};
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub type RunResult = Result<(), Spanned<RuntimeError>>;

pub trait Executable {
    fn execute(&self, environment: &mut Environment, span: &Span) -> RunResult;
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::ValInt(int) => write!(f, "{int}"),
            Value::ValFloat(float) => write!(f, "{float}"),
            Value::ValChar(c) => write!(f, "#{c}"),
            Value::ValString(str) => write!(f, "\"{str}\""),
            Value::ValQuotedWord(word) => write!(f, "'{word}"),
            Value::ValBlock(words) => {
                write!(f, ")")?;
                for (i, word) in words.iter().enumerate() {
                    write!(f, "{}", word.value)?;

                    if i < words.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            Value::ValArray(words) => {
                write!(f, "[")?;
                for (i, word) in words.iter().enumerate() {
                    write!(f, "{}", word)?;

                    if i < words.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            Value::ValBuiltin(word) => write!(f, "{word}")
        }
    }
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
    fn execute(&self, environment: &mut Environment, span: &Span) -> RunResult {
        match self {
            Word::Int(value) => environment.push(Value::ValInt(value.clone())),
            Word::Float(value) => environment.push(Value::ValFloat(value.clone())),
            Word::Char(value) => environment.push(Value::ValChar(value.clone())),
            Word::String(value) => environment.push(Value::ValString(value.clone())),
            Word::QuotedWord(name) => environment.push(Value::ValQuotedWord(name.clone())),
            Word::Block(words) => environment.push(Value::ValBlock(words.clone())),
            Word::Array(words) => environment.push_array(words)?,
            Word::Word(name) => {
                let variable = environment.get_variable(name, span)?;
                environment.execute(variable, span)?
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

pub struct Environment {
    pub stack: Rc<RefCell<Vec<Value>>>,
    pub variables: HashMap<String, Value>,
}

impl Clone for Environment {
    fn clone(&self) -> Self {
        let stack = Rc::clone(&self.stack);
        let variables = self.variables.clone();
        Self { stack, variables }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            stack: Rc::new(RefCell::new(Vec::new())),
            variables: HashMap::from([
                ("+".into(), Value::ValBuiltin(Builtin::Add)),
                ("-".into(), Value::ValBuiltin(Builtin::Sub)),
                ("*".into(), Value::ValBuiltin(Builtin::Mul)),
                ("/".into(), Value::ValBuiltin(Builtin::Div)),
                ("&".into(), Value::ValBuiltin(Builtin::And)),
                ("|".into(), Value::ValBuiltin(Builtin::Or)),
                ("^".into(), Value::ValBuiltin(Builtin::Xor)),
                ("!".into(), Value::ValBuiltin(Builtin::Not)),
                (">".into(), Value::ValBuiltin(Builtin::Greater)),
                (">=".into(), Value::ValBuiltin(Builtin::GreaterOrEqual)),
                ("<".into(), Value::ValBuiltin(Builtin::Less)),
                ("<=".into(), Value::ValBuiltin(Builtin::LessOrEqual)),
                ("=".into(), Value::ValBuiltin(Builtin::Equal)),
                ("!=".into(), Value::ValBuiltin(Builtin::NotEqual)),
                ("++".into(), Value::ValBuiltin(Builtin::Concat)),
                ("@".into(), Value::ValBuiltin(Builtin::Read)),
                ("$".into(), Value::ValBuiltin(Builtin::Write)),
                ("%".into(), Value::ValBuiltin(Builtin::Execute)),
                (".".into(), Value::ValBuiltin(Builtin::Print)),
                ("?".into(), Value::ValBuiltin(Builtin::Stack)),
                ("neg".into(), Value::ValBuiltin(Builtin::Neg)),
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
                ("mod".into(), Value::ValBuiltin(Builtin::Rem)),
                ("max".into(), Value::ValBuiltin(Builtin::Max)),
                ("min".into(), Value::ValBuiltin(Builtin::Min)),
                ("fold".into(), Value::ValBuiltin(Builtin::Fold)),
                ("reduce".into(), Value::ValBuiltin(Builtin::Reduce)),
            ]),
        }
    }
}

impl Environment {
    pub fn push(&mut self, value: Value) {
        self.stack.borrow_mut().push(value)
    }

    pub fn pop(&mut self, span: &Span) -> Result<Value, Spanned<RuntimeError>> {
        self.stack.borrow_mut().pop().ok_or_else(|| Spanned::new(RuntimeError::EmptyStack, span.clone()))
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
        let array_start_stack_idx = self.stack.borrow_mut().len();

        for Spanned { value: word, span } in words {
            word.execute(self, span)?
        }

        let elements = self.stack.borrow_mut().drain(array_start_stack_idx..).collect();
        self.push(Value::ValArray(elements));
        Ok(())
    }

    pub fn get_variable(&self, name: &String, span: &Span) -> Result<Value, Spanned<RuntimeError>> {
        match self.variables.get(name) {
            Some(value) => Ok(value.clone()),
            None => Err(Spanned::new(RuntimeError::UnknownWord(name.clone()), span.clone()))
        }
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn execute(&mut self, value: Value, span: &Span) -> RunResult {
        match value {
            Value::ValBlock(words) => {
                let mut new_environment = self.clone();

                for Spanned { value: word, span } in words {
                    word.execute(&mut new_environment, &span)?
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

struct Interpreter {
    words: VecDeque<Spanned<Word>>,
    environment: Environment,
}

impl Interpreter {
    fn new(words: Vec<Spanned<Word>>) -> Self {
        Self {
            words: words.into_iter().collect(),
            environment: Default::default(),
        }
    }

    fn run(mut self) -> Result<Vec<Value>, Vec<Spanned<RuntimeError>>> {
        while let Some(word) = self.words.pop_front() {
            if let Err(error) = word.value.execute(&mut self.environment, &word.span) {
                return Err(vec![error]);
            }
        }

        Ok(self.environment.stack.take())
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
