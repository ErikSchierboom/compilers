use crate::location::{Span, Spanned};
use crate::parser::{parse, ParseError, ParseResult, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
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
    Array(Vec<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{c}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::Integer(int) => write!(f, "{int}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Lambda(words) => write!(f, "({})", words.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
            Value::Array(elements) => write!(f, "({})", elements.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
        }
    }
}

pub struct Environment {
    stack: Vec<Value>,
    span: Span,
}

impl Environment {
    fn new() -> Self {
        Self { stack: Vec::new(), span: Span::EMPTY }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| self.spanned(RuntimeError::EmptyStack))
    }

    fn pop_n(&mut self, n: usize) -> InterpretResult<Vec<Value>> {
        if n <= self.stack.len() {
            Ok(self.stack.drain((self.stack.len() - n)..).collect())
        } else {
            Err(self.spanned(RuntimeError::EmptyStack))
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

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }

    fn make_error(&self, error: RuntimeError) -> Spanned<RuntimeError> {
        self.spanned(error)
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
            Word::Array(array) => {
                for element in array {
                    element.value.execute(env)?;
                }

                let values = env.pop_n(array.len())?;
                env.push(Value::Array(values))
            }
            Word::Lambda(words) => env.push(Value::Lambda(words.iter().map(|word| word.value.clone()).collect())),
            Word::Identifier(identifier) => {
                todo!()
            }
            Word::Niladic(_) => todo!(),
            Word::Monadic(_) => todo!(),
            Word::Dyadic(_) => todo!()
        }

        Ok(())
    }
}

pub type InterpretResult<T = ()> = Result<T, Spanned<RuntimeError>>;

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
                Err(error) => {
                    self.environment.span = error.span.clone();
                    return Err(self.environment.make_error(RuntimeError::Parse(error.value)));
                }
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
