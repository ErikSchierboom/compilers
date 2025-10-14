use crate::array::{Array, Shape};
use crate::parser::{parse, DyadicOperation, MonadicOperation, NiladicOperation, ParseError, ParseResult, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    UnsupportedArgumentTypes, // TODO: store allows argument types
    IncompatibleArrayShapes,
    IncompatibleArrayValues,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::UnsupportedArgumentTypes => write!(f, "Unsupported argument types"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
            RuntimeError::IncompatibleArrayValues => write!(f, "Incompatible array values"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Char(char),
    Float(f64),
    Integer(i64),
    String(String),
    Lambda(Vec<Word>),
    Array(Array<Value>),
}

impl Value {
    pub fn as_char(&self) -> Option<&char> {
        match self {
            Value::Char(c) => Some(c),
            _ => None
        }
    }

    pub fn as_float(&self) -> Option<&f64> {
        match self {
            Value::Float(float) => Some(float),
            _ => None
        }
    }

    pub fn as_integer(&self) -> Option<&i64> {
        match self {
            Value::Integer(int) => Some(int),
            _ => None
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match self {
            Value::String(string) => Some(string),
            _ => None
        }
    }

    fn shape(&self) -> Shape {
        match self {
            Value::Array(array) => array.shape.clone(),
            _ => Shape::empty()
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{c}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::Integer(int) => write!(f, "{int}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Lambda(words) => write!(f, "({})", words.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
            Value::Array(array) => write!(f, "{array}"),
        }
    }
}

pub struct Environment {
    stack: Vec<Value>,
}

impl Environment {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    fn pop_n(&mut self, n: usize) -> InterpretResult<Vec<Value>> {
        if n <= self.stack.len() {
            Ok(self.stack.drain((self.stack.len() - n)..).collect())
        } else {
            Err(RuntimeError::EmptyStack)
        }
    }

    pub(crate) fn execute_monadic(
        &mut self,
        f: fn(Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        self.push(f(a, self)?);
        Ok(())
    }

    pub(crate) fn execute_dyadic(
        &mut self,
        f: fn(Value, Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(f(a, b, self)?);
        Ok(())
    }
}

pub trait Executable {
    fn execute(&self, env: &mut Environment) -> InterpretResult;
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Integer(int) => env.push(Value::Integer(int.clone())),
            Word::Float(float) => env.push(Value::Float(float.clone())),
            Word::String(string) => env.push(Value::String(string.clone())),
            Word::Char(c) => env.push(Value::Char(c.clone())),
            Word::Array(elements) => {
                let array = if elements.is_empty() {
                    Array::empty()
                } else {
                    for element in elements {
                        element.value.execute(env)?;
                    }

                    let values = env.pop_n(elements.len())?;

                    for window in values.windows(2) {
                        if window[0].shape() != window[1].shape() {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }
                        
                        // TODO: ensure array is homogenous
                    }

                    let shape = values.first().unwrap().shape();
                    Array::new(shape, values)
                };

                env.push(Value::Array(array))
            }
            Word::Lambda(words) => env.push(Value::Lambda(words.iter().map(|word| word.value.clone()).collect())),
            Word::Identifier(identifier) => {
                todo!()
            }
            Word::Niladic(niladic_op) => niladic_op.execute(env)?,
            Word::Monadic(monadic_op) => monadic_op.execute(env)?,
            Word::Dyadic(dyadic_op) => dyadic_op.execute(env)?,
        }

        Ok(())
    }
}

impl Executable for NiladicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            NiladicOperation::Stack => {
                for value in env.stack.iter().rev() {
                    println!("{value}")
                }
                Ok(())
            }
        }
    }
}

impl Executable for MonadicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            MonadicOperation::Not => {
                todo!("implement not")
            }
        }
    }
}

impl Executable for DyadicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            DyadicOperation::Add => {
                let right_val = env.pop()?;
                let left_val = env.pop()?;

                match (left_val, right_val) {
                    (Value::Integer(left), Value::Integer(right)) => {
                        env.push(Value::Integer(left + right));
                        Ok(())
                    }
                    (Value::Integer(left), Value::Array(mut right)) => {
                        for right_element in right.elements.iter_mut() {
                            let right_int = right_element.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Integer(left + right_int)
                        }

                        env.push(Value::Array(right));
                        Ok(())
                    }
                    _ => Err(RuntimeError::UnsupportedArgumentTypes)
                }
            }
            DyadicOperation::Sub => todo!(),
            DyadicOperation::Mul => todo!(),
            DyadicOperation::Div => todo!()
        }
    }
}

pub type InterpretResult<T = ()> = Result<T, RuntimeError>;

pub struct Interpreter<T>
where
    T: Iterator<Item=ParseResult>,
{
    words: Peekable<T>,
    environment: Environment,
}

impl<T> Interpreter<T>
where
    T: Iterator<Item=ParseResult>,
{
    pub fn new(words: T) -> Self {
        Self { words: words.peekable(), environment: Environment::new() }
    }

    pub fn interpret(&mut self) -> InterpretResult<Vec<Value>> {
        while let Some(parse_result) = self.next() {
            match parse_result {
                Ok(word) => word.value.execute(&mut self.environment)?,
                Err(error) => return Err(RuntimeError::Parse(error.value))
            }
        }

        Ok(self.environment.stack.clone())
    }

    fn next(&mut self) -> Option<ParseResult> {
        self.words.next()
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult<Vec<Value>> {
    let words = parse(source);
    let mut interpreter = Interpreter::new(words);
    interpreter.interpret()
}
