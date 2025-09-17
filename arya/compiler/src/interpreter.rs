use crate::array::{Array, Shape};
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
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::NonRectangularArray => write!(f, "Non rectangular array")
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Numbers(Array<i64>)
}

macro_rules! dyadic_value_operation {
    ($name:ident, $operation:tt) => {
        impl Value {
            fn $name(a: Value, b: Value) -> InterpretResult<Value> {
                match (a, b) {
                    (Value::Numbers(array_a), Value::Numbers(array_b)) => {
                        // TODO: check shapes
                        // TODO: maybe move some functionality to array type
                        let updated_values = array_a.values.into_iter().zip(array_b.values).map(|(l, r)| l $operation r).collect();
                        Ok(Value::Numbers(Array::new(array_a.shape, updated_values)))
                    }
                }
            }
        }
    };
}

macro_rules! monadic_value_operation {
    ($name:ident, $operation:tt) => {
        impl Value {
            fn $name(a: Value) -> InterpretResult<Value> {
                match a {
                    Value::Numbers(array_a) => {
                        // TODO: check shapes
                        // TODO: maybe move some functionality to array type
                        let updated_values = array_a.values.into_iter().map(|v| $operation v).collect();
                        Ok(Value::Numbers(Array::new(array_a.shape, updated_values)))
                    }
                }
            }
        }
    };
}

dyadic_value_operation!(add, +);
dyadic_value_operation!(subtract, -);
dyadic_value_operation!(multiply, *);
dyadic_value_operation!(divide, /);
dyadic_value_operation!(xor, ^);
dyadic_value_operation!(and, &);
dyadic_value_operation!(or, |);

monadic_value_operation!(not, !);
monadic_value_operation!(negate, -);

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

    pub(crate) fn execute_dyadic(
        &mut self,
        f: fn(Value, Value) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(f(a, b)?);
        Ok(())
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

impl Executable for Primitive {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Primitive::Add => env.execute_dyadic(Value::add)?,
            Primitive::Subtract => env.execute_dyadic(Value::subtract)?,
            Primitive::Multiply => env.execute_dyadic(Value::multiply)?,
            Primitive::Divide => env.execute_dyadic(Value::divide)?,
            Primitive::Xor => env.execute_dyadic(Value::xor)?,
            Primitive::And => env.execute_dyadic(Value::and)?,
            Primitive::Or => env.execute_dyadic(Value::or)?,
            Primitive::Not => env.execute_monadic(Value::not)?,
            Primitive::Negate => env.execute_monadic(Value::negate)?,
            Primitive::Equal => todo!(),
            Primitive::NotEqual => todo!(),
            Primitive::Greater => todo!(),
            Primitive::GreaterEqual => todo!(),
            Primitive::Less => todo!(),
            Primitive::LessEqual => todo!(),
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
            Word::Integer(i) => {
                // TODO: maybe add function to more easily create scalar
                env.push(Value::Numbers(Array::new(Shape::Scalar, vec![i.clone()])))
            },
            Word::Primitive(primitive) => return primitive.execute(env),
            Word::Array(array) => {
                if array.iter().all(|element| matches!(element.value, Word::Integer(_))) {
                    todo!()
                } else if array.iter().all(|element| matches!(element.value, Word::Array(_))) {
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
