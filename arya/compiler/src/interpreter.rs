use crate::array::{Array, Shape};
use crate::location::{Span, Spanned};
use crate::parser::{parse, ParseError, ParseResult, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    MissingArgument,
    NonRectangularArray,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::MissingArgument => write!(f, "Missing argument"),
            RuntimeError::NonRectangularArray => write!(f, "Non rectangular array")
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Numbers(Array<i64>)
}

pub struct Environment {
    stack: Vec<Value>,
    span: Span,
}

impl Environment {
    fn new() -> Self {
        Self { stack: Vec::new(), span: Span::EMPTY }
    }

    fn push(&mut self, value: Value) -> InterpretResult {
        self.stack.push(value);
        Ok(())
    }

    fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| self.spanned(RuntimeError::MissingArgument))
    }

    fn pop_map<V>(
        &mut self,
        f: impl FnOnce(&mut Self, Value) -> InterpretResult<V>,
    ) -> InterpretResult<V> {
        let value = self.pop()?;
        f(self, value)
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }

    fn make_error(&mut self, error: RuntimeError) -> Spanned<RuntimeError> {
        self.spanned(error)
    }
}

pub trait Executable {
    fn execute(&self, env: &mut Environment) -> InterpretResult;
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Integer(i) => {
                env.push(Value::Numbers(Array::new(Shape::Scalar, vec![i.clone()])))
            }
            Word::Primitive(_) => todo!(),
            Word::Array(array) => {
                if array.iter().all(|element| matches!(element.value, Word::Integer(_))) {
                    todo!()
                } else if array.iter().all(|element| matches!(element.value, Word::Integer(_))) {
                    todo!()
                } else {
                    Err(env.make_error(RuntimeError::NonRectangularArray))
                }
            }
            Word::Lambda(_) => todo!(),
        }
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
