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
                ("+".to_string(), Value::Procedure(Procedure::Native(native_plus))),
                ("define".to_string(), Value::Procedure(Procedure::Native(native_define)))
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
    InvalidArgumentType,
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
    Native(fn(Vec<&Value>, &mut Environment) -> Result<Value, RuntimeError>)
}

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
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
            Node::Bool(b) => Value::Bool(b),
            Node::Char(c) => Value::Char(c),
            Node::String(string) => Value::String(string),
            Node::Symbol(symbol) => Value::Symbol(symbol),
            Node::List(elements) => Value::List(elements.into_iter().map(Value::from).collect())
        }
    }
}

fn native_plus(args: Vec<&Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
    let sum = 0i64;

    args.iter().try_fold(sum, |acc, arg| {
        let mut arg_env = env.clone();
        let evaluated_arg = evaluate(arg, &mut arg_env)?;
        match evaluated_arg {
            Value::Integer(j) => Ok(acc + j),
            _ => Err(RuntimeError::InvalidArgumentType)
        }
    }).map(Value::Integer)
}

fn native_define(args: Vec<&Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
    println!("{:?}", args);
    println!("{:?}", env);

    if args.len() != 2 {
        return Err(RuntimeError::InvalidArgumentType)
    }

    match args[0] {
        Value::Symbol(name) => {
            let arg = args[1];
            let evaluated_arg = evaluate(arg, env)?;
            env.set(name.to_string(), evaluated_arg);
            Ok(Value::List(Vec::new()))
        }
        _ => Err(RuntimeError::InvalidArgumentType)
    }
}

pub fn evaluate_list(values: Vec<Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
    let mut result = Value::List(Vec::new());

    for value in values {
        result = evaluate(&value, env)?
    }

    Ok(result)
}

pub fn evaluate(value: &Value, env: &mut Environment) -> Result<Value, RuntimeError> {
    match value {
        &Value::Integer(i) => Ok(Value::Integer(i)),
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
                let symbol = evaluate(elements.get(0).unwrap(), env)?;
                match symbol {
                    Value::Procedure(procedure) => {
                        let args = elements[1..].into_iter().collect();
                        evaluate_procedure(procedure, args, env)
                    },
                    _ => Err(RuntimeError::ExpectedProcedure(symbol))
                }
            }
        },
        &Value::Procedure(_) => todo!()
    }
}

fn evaluate_procedure(procedure: Procedure, args: Vec<&Value>, env: &mut Environment) -> Result<Value, RuntimeError> {
    match procedure {
        Procedure::Lambda(parameters, body, closure_env) => {
            if args.len() != parameters.len() {
                return Err(RuntimeError::InvalidNumberOfArguments)
            }

            let mut lambda_env = closure_env.create_child();
            for (parameter, arg) in parameters.iter().zip(args) {
                let arg_value = evaluate(arg, env)?;
                lambda_env.set(parameter.clone(), arg_value)
            }

            evaluate_list(body, &mut lambda_env)
        },
        Procedure::Native(func) => {
            func(args, env)
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
        let mut env = Environment::default();
        evaluate_list(self.values.clone(), &mut env)
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
