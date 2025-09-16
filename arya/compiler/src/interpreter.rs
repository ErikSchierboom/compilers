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
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::MissingArgument => write!(f, "Missing argument")
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Numbers(Array<i64>)
}

pub fn interpret<'a>(source: &str) -> InterpretResult<Vec<Value>> {
    let words = parse(source);
    let mut interpreter = Interpreter::new(words);
    interpreter.interpret()
}
pub type InterpretResult<T = ()> = Result<T, Spanned<RuntimeError>>;

pub struct Interpreter<T>
where
    T: Iterator<Item=ParseResult>,
{
    words: Peekable<T>,
    stack: Vec<Value>,
    span: Span,
}

pub trait Executable {
    fn execute<T>(&self, interpreter: &mut Interpreter<T>) -> InterpretResult
    where
        T: Iterator<Item=ParseResult>,
    {
        todo!()
    }
}

impl Executable for Word {
    fn execute<T>(&self, interpreter: &mut Interpreter<T>) -> InterpretResult
    where
        T: Iterator<Item=ParseResult>,
    {
        match self {
            Word::Integer(i) => {
                interpreter.push(Value::Numbers(Array::new(Shape::Scalar, vec![i.clone()])));
                Ok(())
            }
            Word::Primitive(_) => todo!(),
            Word::Array(_) => todo!(),
            Word::Lambda(_) => todo!(),
        }
    }
}

impl<T> Interpreter<T>
where
    T: Iterator<Item=ParseResult>,
{
    pub fn new(words: T) -> Self {
        let words = words.peekable();

        Self { words, stack: Vec::new(), span: Span::EMPTY }
    }

    pub fn interpret(&mut self) -> InterpretResult<Vec<Value>> {
        while let Some(parse_result) = self.next() {
            match parse_result {
                Ok(word) => word.value.execute(self)?,
                Err(error) => {
                    self.span = error.span.clone();
                    return Err(self.make_error(RuntimeError::Parse(error.value)));
                }
            }
        }

        Ok(self.stack.clone())
    }

    fn next(&mut self) -> Option<ParseResult> {
        self.words.next()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| self.spanned(RuntimeError::MissingArgument))
    }

    pub fn pop_map<V>(
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
