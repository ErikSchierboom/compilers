use crate::array::Array;
use crate::location::{Span, Spanned};
use crate::parser::{parse, ParseError, ParseResult, Primitive, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    NonRectangularArray,
    IncompatibleArrayShapes,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::NonRectangularArray => write!(f, "Non rectangular array"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Numbers(Array<i64>)
}

macro_rules! dyadic_operation_env {
    ($name:ident, $operation:tt) => {
        impl Value {
            fn $name(a: Value, b: Value, env: &Environment) -> InterpretResult<Value> {
                match (a, b) {
                    (Value::Numbers(array_a), Value::Numbers(array_b)) => {
                        let mapped_array = if array_a.shape.is_scalar() {
                            let scalar = array_a.values.first().unwrap();
                            let mapped_values = array_b.values.iter().map(|b| (*scalar $operation *b) as i64).collect();
                            Array::new(array_b.shape.clone(), mapped_values)
                        } else if array_b.shape.is_scalar() {
                            let scalar = array_b.values.first().unwrap();
                            let mapped_values = array_a.values.iter().map(|a| (*a $operation *scalar) as i64).collect();
                            Array::new(array_a.shape.clone(), mapped_values)
                        } else if array_a.shape == array_b.shape {
                            let mapped_values = array_a.values.iter().zip(&array_b.values).map(|(a, b)| (*a $operation *b) as i64).collect();
                            Array::new(array_a.shape.clone(), mapped_values)
                        } else {
                            return Err(env.make_error(RuntimeError::IncompatibleArrayShapes))
                        };

                        Ok(Value::Numbers(mapped_array))
                    }
                }
            }
        }
    };
}

macro_rules! monadic_operation {
    ($name:ident, $operation:tt) => {
        impl Value {
            fn $name(a: Value) -> InterpretResult<Value> {
                match a {
                    Value::Numbers(array_a) => {
                        let mapped_values = array_a.values.into_iter().map(|v| ($operation v) as i64).collect();
                        Ok(Value::Numbers(Array::new(array_a.shape, mapped_values)))
                    }
                }
            }
        }
    };
}

dyadic_operation_env!(add, +);
dyadic_operation_env!(subtract, -);
dyadic_operation_env!(multiply, *);
dyadic_operation_env!(divide, /);
dyadic_operation_env!(xor, ^);
dyadic_operation_env!(and, &);
dyadic_operation_env!(or, |);
dyadic_operation_env!(equal, ==);
dyadic_operation_env!(not_equal, !=);
dyadic_operation_env!(greater, >);
dyadic_operation_env!(greater_equal, >=);
dyadic_operation_env!(less, <);
dyadic_operation_env!(less_equal, <=);

monadic_operation!(not, !);
monadic_operation!(negate, -);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Numbers(array) => write!(f, "{}", array)
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

    pub(crate) fn execute_monadic(
        &mut self,
        f: fn(Value) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        self.push(f(a)?);
        Ok(())
    }

    pub(crate) fn execute_dyadic_env(
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

impl Executable for Primitive {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Primitive::Add => env.execute_dyadic_env(Value::add)?,
            Primitive::Subtract => env.execute_dyadic_env(Value::subtract)?,
            Primitive::Multiply => env.execute_dyadic_env(Value::multiply)?,
            Primitive::Divide => env.execute_dyadic_env(Value::divide)?,
            Primitive::Xor => env.execute_dyadic_env(Value::xor)?,
            Primitive::And => env.execute_dyadic_env(Value::and)?,
            Primitive::Or => env.execute_dyadic_env(Value::or)?,
            Primitive::Not => env.execute_monadic(Value::not)?,
            Primitive::Negate => env.execute_monadic(Value::negate)?,
            Primitive::Equal => env.execute_dyadic_env(Value::equal)?,
            Primitive::NotEqual => env.execute_dyadic_env(Value::not_equal)?,
            Primitive::Greater => env.execute_dyadic_env(Value::greater)?,
            Primitive::GreaterEqual => env.execute_dyadic_env(Value::greater_equal)?,
            Primitive::Less => env.execute_dyadic_env(Value::less)?,
            Primitive::LessEqual => env.execute_dyadic_env(Value::less_equal)?,
            Primitive::Dup => {
                let a = env.pop()?;
                env.push(a.clone());
                env.push(a);
            }
            Primitive::Drop => {
                env.pop()?;
            }
            Primitive::Swap => {
                let a = env.pop()?;
                let b = env.pop()?;
                env.push(a);
                env.push(b);
            }
            Primitive::Over => {
                let a = env.pop()?;
                let b = env.pop()?;
                env.push(b.clone());
                env.push(a);
                env.push(b);
            }
            Primitive::Reduce => todo!()
        }

        Ok(())
    }
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Integer(i) => env.push(Value::Numbers(Array::scalar(i.clone()))),
            Word::Primitive(primitive) => return primitive.execute(env),
            Word::Array(array) => {
                if array.values.iter().all(|element| matches!(element.value, Word::Integer(_))) {
                    let values = array.values.iter().map(|spanned_word| spanned_word.value.as_integer().unwrap());
                    let array = Value::Numbers(Array::linear(values.collect()));
                    env.push(array)
                } else if array.values.iter().all(|element| matches!(element.value, Word::Array(_))) {
                    todo!()
                } else {
                    return Err(env.make_error(RuntimeError::NonRectangularArray))
                }
            }
            Word::Lambda(_) => todo!(),
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
