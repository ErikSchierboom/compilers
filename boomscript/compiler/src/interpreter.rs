use crate::environment::Environment;
use crate::parser::{parse, ParseError, Word};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    UnknownIdentifier(String),
    EmptyStack,
}

#[derive(Clone, Debug)]
pub enum Value {
    Char(char),
    Number(i64),
    String(String),
    Identifier(String),
    Block(Vec<Word>),
    Builtin(Builtin),
}

#[derive(Clone, Debug)]
pub enum Builtin {
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Dup,
    Drop,
}

pub trait Executable {
    fn execute(&self, env: &mut Environment) -> Result<(), RuntimeError>;
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> Result<(), RuntimeError> {
        match self {
            Word::Number(n) => env.push(Value::Number(n.clone())),
            Word::Char(c) => env.push(Value::Char(c.clone())),
            Word::String(str) => env.push(Value::String(str.clone())),
            Word::Identifier(name) => {
                let value = env.get(name)?.clone();
                value.execute(env)?;
            }
            Word::Quote(name) => env.push(Value::Identifier(name.clone())),
            Word::Block(words) => env.push(Value::Block(words.clone())),
        }

        Ok(())
    }
}

impl Executable for Value {
    fn execute(&self, env: &mut Environment) -> Result<(), RuntimeError> {
        match self {
            Value::Char(_) => {}
            Value::Number(_) => {}
            Value::String(_) => {}
            Value::Identifier(_) => {}
            Value::Block(words) => {
                for word in words {
                    word.execute(env)?;
                }
            }
            Value::Builtin(_) => {}
        }

        Ok(())
    }
}

pub struct Interpreter<T>
where
    T: Iterator<Item=Word>,
{
    words: Peekable<T>,
    environment: Environment,
}

impl<T> Interpreter<T>
where
    T: Iterator<Item=Word>,
{
    pub fn new(words: T) -> Self {
        Self { words: words.peekable(), environment: Environment::new() }
    }

    pub fn interpret(&mut self) -> Result<Value, RuntimeError> {
        while let Some(word) = self.words.next() {
            word.execute(&mut self.environment)?;
        }

        self.environment.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }
}

pub fn interpret(source: &str) -> Result<Value, RuntimeError> {
    let words = parse(source).map_err(RuntimeError::Parse)?;
    let mut interpreter = Interpreter::new(words.into_iter());
    interpreter.interpret()
}