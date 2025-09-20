use crate::array::{Array, Shape};
use crate::location::{Span, Spanned};
use crate::parser::{parse, Modifier, ParseError, ParseResult, Primitive, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    IncompatibleArrayShapes,
    CannotReduceEmptyArray,
    ExpectedLogicalArray,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
            RuntimeError::CannotReduceEmptyArray => write!(f, "Cannot reduce empty array"),
            RuntimeError::ExpectedLogicalArray => write!(f, "Expected logical array"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Numbers(Array<i64>)
}

impl Value {
    pub fn shape(&self) -> &Shape {
        match self {
            Value::Numbers(array) => &array.shape
        }
    }

    pub fn as_numbers(&self) -> Option<&Vec<i64>> {
        match self {
            Value::Numbers(array) => Some(&array.values)
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Numbers(array) => write!(f, "{}", array)
        }
    }
}

macro_rules! dyadic_operation_env {
    ($name:ident, $operation:tt) => {
        impl Value {
            fn $name(a: Value, b: Value, env: &Environment) -> InterpretResult<Value> {
                match (a, b) {
                    (Value::Numbers(array_a), Value::Numbers(array_b)) => {
                        let mapped_array = if array_a.shape.is_scalar() {
                            let scalar = array_a.values.first().unwrap();
                            let mapped_values = array_b.values.iter().map(|&b| (*scalar $operation b) as i64).collect();
                            Array::new(array_b.shape.clone(), mapped_values)
                        } else if array_b.shape.is_scalar() {
                            let scalar = array_b.values.first().unwrap();
                            let mapped_values = array_a.values.iter().map(|&a| (a $operation *scalar) as i64).collect();
                            Array::new(array_a.shape.clone(), mapped_values)
                        } else if array_a.shape == array_b.shape {
                            let mapped_values = array_a.values.iter().zip(&array_b.values).map(|(&a, &b)| (a $operation b) as i64).collect();
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
            fn $name(value: Value) -> InterpretResult<Value> {
                match value {
                    Value::Numbers(array) => {
                        let mapped_values = array.values.into_iter().map(|v| ($operation v) as i64).collect();
                        Ok(Value::Numbers(Array::new(array.shape, mapped_values)))
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

monadic_operation!(negate, -);

macro_rules! monadic_method {
    ($name:ident, $operation:ident) => {
        impl Value {
            fn $name(value: Value) -> InterpretResult<Value> {
                match value {
                    Value::Numbers(mut array) => {
                        array.values.$operation();
                        Ok(Value::Numbers(array))
                    }
                }
            }
        }
    };
}

monadic_method!(reverse, reverse);

impl Value {
    fn not(value: Value) -> InterpretResult<Value> {
        match value {
            Value::Numbers(mut array) => {
                for value in array.values.iter_mut() {
                    *value = if *value == 0 { 1 } else { 0 }
                }
                Ok(Value::Numbers(array))
            }
        }
    }

    fn keep(a: Value, b: Value, env: &Environment) -> InterpretResult<Value> {
        match (a, b) {
            (Value::Numbers(array_a), Value::Numbers(array_b)) => {
                if !array_a.shape.is_one_dimensional() {
                    Err(env.make_error(RuntimeError::ExpectedLogicalArray))
                } else {
                    let new_rows: Vec<Vec<i64>> = array_b
                        .row_slices()
                        .into_iter()
                        .zip(&array_a.values)
                        .filter_map(|(row, &keep)| {
                            if keep == 0 {
                                None
                            } else {
                                Some(row.iter().copied().collect())
                            }
                        })
                        .collect();

                    let mut new_shape = array_b.shape.clone();
                    new_shape.replace_dimension(0, new_rows.len());
                    Ok(Value::Numbers(Array::new(new_shape, new_rows.into_iter().flatten().collect())))
                }
            }
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

    pub fn pop_map<T>(
        &mut self,
        f: impl FnOnce(&Value, &mut Self) -> InterpretResult<T>,
    ) -> InterpretResult<T> {
        f(&self.pop()?, self)
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

    pub fn make_array(&mut self, size: usize) -> InterpretResult<Value> {
        let pushed_values = self.pop_n(size)?;
        if !pushed_values.windows(2).all(|window| window[0].shape() == window[1].shape()) {
            return Err(self.make_error(RuntimeError::IncompatibleArrayShapes));
        }

        let array = if let Some(first) = pushed_values.first() {
            let numbers: Vec<i64> = pushed_values.iter()
                .map(|value| value.as_numbers().unwrap().clone())
                .flatten()
                .collect();
            let mut shape = first.shape().clone();
            shape.prepend_dimension(size);

            Array::new(shape, numbers)
        } else {
            Array::empty()
        };

        Ok(Value::Numbers(array))
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
            Primitive::Reverse => env.execute_monadic(Value::reverse)?,
            Primitive::Keep => env.execute_dyadic_env(Value::keep)?
        }

        Ok(())
    }
}

impl Executable for Modifier {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Modifier::Reduce(lambda) => {
                let value = env.pop()?;

                match value {
                    Value::Numbers(array) => {
                        let first_value = array.values.first().ok_or_else(|| env.make_error(RuntimeError::CannotReduceEmptyArray))?;
                        env.push(Value::Numbers(Array::scalar(first_value.clone())));

                        for value in array.values.iter().skip(1) {
                            env.push(Value::Numbers(Array::scalar(value.clone())));

                            for word in &lambda.value.body {
                                word.value.execute(env)?;
                            }
                        }
                    }
                }
            }
            Modifier::Fold(lambda) => {
                let initial = env.pop()?;
                let value = env.pop()?;

                match value {
                    Value::Numbers(array) => {
                        env.push(initial);

                        for value in array.values {
                            env.push(Value::Numbers(Array::scalar(value.clone())));

                            for word in &lambda.value.body {
                                word.value.execute(env)?;
                            }
                        }
                    }
                }
            }
            Modifier::Both(lambda) => {
                let b = env.pop()?;
                let a = env.pop()?;

                env.push(a);
                for word in &lambda.value.body {
                    word.value.execute(env)?;
                }

                env.push(b);
                for word in &lambda.value.body {
                    word.value.execute(env)?;
                }
            }
        }

        Ok(())
    }
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Integer(i) => {
                let value = Value::Numbers(Array::scalar(i.clone()));
                env.push(value)
            }
            Word::Array(array) => {
                let stack_count_before = env.stack.len();

                for spanned_word in &array.values {
                    spanned_word.value.execute(env)?;
                }

                let stack_count_after = env.stack.len();

                let value = env.make_array(stack_count_after - stack_count_before)?;
                env.push(value)
            }
            Word::Primitive(primitive) => return primitive.execute(env),
            Word::Modifier(modifier) => return modifier.execute(env),
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
