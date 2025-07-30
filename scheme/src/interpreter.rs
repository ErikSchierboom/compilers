use crate::parser::{parse, Node};
use crate::scanner::SyntaxError;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    MissingFunction,
    ExpectedInteger,
    UnknownFunction(String)
}

#[derive(Debug, Clone)]
pub enum Error {
    Syntax(SyntaxError),
    Runtime(RuntimeError)
}

struct Interpreter {
    values: Vec<Value>,
    stack: Vec<Value>,
    current: usize,
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Value>),
}

impl From<Node> for Value {
    fn from(value: Node) -> Self {
        match value {
            Node::Integer(i) => Value::Integer(i),
            Node::Float(f) => Value::Float(f),
            Node::Bool(b) => Value::Bool(b),
            Node::Char(c) => Value::Char(c),
            Node::String(string) => Value::String(string),
            Node::Symbol(symbol) => Value::Symbol(symbol),
            Node::List(elements) => Value::List(elements.into_iter().map(Value::from).collect())
        }
    }
}

pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Procedure>
}

pub enum Procedure {
    Lambda(Vec<String>, Vec<Value>, Environment),
    Builtin(fn(Vec<Value>, Environment) -> Result<Value, RuntimeError>)
}

impl Environment {
    pub fn default() -> Self {
        Self { parent: None, variables: HashMap::new() }
    }

    pub fn create_child(self) -> Self {
        Self { parent: Some(Box::new(self)), variables: HashMap::new() }
    }

    // TODO: get by name
    // TODO: set by name
}

impl Interpreter {
    fn new(nodes: Vec<Node>) -> Self {
        Self { values: nodes.into_iter().map(Value::from).collect(), stack: Vec::new(), current: 0 }
    }

    pub fn interpret(&mut self) -> Result<Value, RuntimeError> {
        let env = Environment::default();
        let mut result = Value::List(Vec::new());

        for value in self.values.clone() {
            result = self.evaluate(&value, &env)?
        }

       Ok(result)
    }

    pub fn evaluate(&mut self, value: &Value, env: &Environment) -> Result<Value, RuntimeError> {
        Ok(value.clone())
    }
}

pub fn interpret(source_code: &str) -> Result<Value, Error> {
    match parse(source_code) {
        Ok(nodes) => {
            let mut interpreter = Interpreter::new(nodes);
            match interpreter.interpret() {
                Ok(value) => Ok(value),
                Err(error) => Err(Error::Runtime(error))
            }
        }
        Err(error) => Err(Error::Syntax(error))
    }
}
