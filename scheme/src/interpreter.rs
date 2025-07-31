use crate::parser::{parse, Node};
use crate::scanner::SyntaxError;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Environment {
    parent: Option<Box<Environment>>,
    variables: HashMap<String, Value>
}

impl Environment {
    pub fn default() -> Self {
        Self {
            parent: None,
            variables: HashMap::from([
                ("+".to_string(), Value::Procedure(Procedure::Native(Interpreter::native_plus)))
            ])
        }
    }

    pub fn create_child(&self) -> Self {
        Self { parent: Some(Box::new(self.clone())), variables: HashMap::new() }
    }

    pub fn get(&self, name: &String) -> Option<&Value> {
        match self.variables.get(name) {
            Some(value) => Some(value),
            None => match &self.parent {
                Some(boxed) => boxed.get(name),
                None => None
            }
        }
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    UnknownSymbol(String),
    ExpectedProcedure(Value),
    UnknownArgument,
    InvalidNumberOfArguments,
}

#[derive(Debug, Clone)]
pub enum Error {
    Syntax(SyntaxError),
    Runtime(RuntimeError)
}

#[derive(Debug, Clone)]
pub enum Procedure {
    Lambda(Vec<String>, Vec<Value>, Environment),
    Native(fn(Vec<&Value>, &Environment) -> Result<Value, RuntimeError>)
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
    Procedure(Procedure)
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



struct Interpreter {
    values: Vec<Value>
}

impl Interpreter {
    fn new(nodes: Vec<Node>) -> Self {
        Self { values: nodes.into_iter().map(Value::from).collect() }
    }

    pub fn interpret(&mut self) -> Result<Value, RuntimeError> {
        let env = Environment::default();
        self.evaluate_list(self.values.clone(), &env)
    }

    pub fn evaluate_list(&mut self, values: Vec<Value>, env: &Environment) -> Result<Value, RuntimeError> {
        let mut result = Value::List(Vec::new());

        for value in values {
            result = self.evaluate(&value, &env)?
        }

        Ok(result)
    }

    pub fn evaluate(&mut self, value: &Value, env: &Environment) -> Result<Value, RuntimeError> {
        match value {
            &Value::Integer(i) => Ok(Value::Integer(i)),
            &Value::Float(f) => Ok(Value::Float(f)),
            &Value::Bool(b) => Ok(Value::Bool(b)),
            &Value::Char(c) => Ok(Value::Char(c)),
            &Value::String(ref string) => Ok(Value::String(string.clone())),
            &Value::Symbol(ref symbol) => {
                match env.get(symbol) {
                    Some(value) => Ok(value.clone()),
                    None => Err(RuntimeError::UnknownSymbol(symbol.clone()))
                }
            },
            &Value::List(ref elements) => {
                if elements.len() == 0 {
                    Ok(Value::List(elements.clone()))
                } else {
                    let symbol = self.evaluate(elements.get(0).unwrap(), env)?;
                    match symbol {
                        Value::Procedure(procedure) => {
                            let args = elements[1..].into_iter().collect();
                            self.evaluate_procedure(procedure, args, &env.create_child())
                        },
                        _ => Err(RuntimeError::ExpectedProcedure(symbol))
                    }
                }
            },
            &Value::Procedure(_) => todo!()
        }
    }

    fn evaluate_procedure(&mut self, procedure: Procedure, args: Vec<&Value>, env: &Environment) -> Result<Value, RuntimeError> {
        match procedure {
            Procedure::Lambda(parameters, body, closure_env) => {
                if args.len() != parameters.len() {
                    return Err(RuntimeError::InvalidNumberOfArguments)
                }

                let mut lambda_env = closure_env.create_child();
                for (parameter, arg) in parameters.iter().zip(args) {
                    match closure_env.get(&parameter) {
                        Some(value) => lambda_env.set(parameter.clone(), value.clone()),
                        None => return Err(RuntimeError::UnknownArgument)
                    }
                }

                self.evaluate_list(body, &lambda_env)
            },
            Procedure::Native(func) => {
                func(args, env)
            }
        }
    }

    fn native_plus(args: Vec<&Value>, env: &Environment) -> Result<Value, RuntimeError> {
        Ok(Value::Integer(0))
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
