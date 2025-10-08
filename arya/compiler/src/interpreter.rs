use crate::array::{Array, ArrayError, Shape};
use crate::location::{Span, Spanned};
use crate::parser::{parse, Modifier, ParseError, ParseResult, Primitive, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Sub};

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    IncompatibleArrayShapes,
    CannotReduceEmptyArray,
    CannotNotCharacter,
    ExpectedLogicalArray,
    ExpectedNumericArray,
    ExpectedScalarNumber,
}

impl From<ArrayError> for RuntimeError {
    fn from(value: ArrayError) -> Self {
        match value {
            ArrayError::IncompatibleShapes => RuntimeError::IncompatibleArrayShapes
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
            RuntimeError::CannotReduceEmptyArray => write!(f, "Cannot reduce empty array"),
            RuntimeError::ExpectedLogicalArray => write!(f, "Expected logical array"),
            RuntimeError::ExpectedNumericArray => write!(f, "Expected numeric array"),
            RuntimeError::CannotNotCharacter => write!(f, "Cannot not character"),
            RuntimeError::ExpectedScalarNumber => write!(f, "Expected scalar number"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone)]
pub enum Value {
    Chars(Array<char>),
    Numbers(Array<i64>),
}

impl Value {
    pub fn shape(&self) -> &Shape {
        match self {
            Value::Numbers(array) => &array.shape,
            Value::Chars(array) => &array.shape
        }
    }

    pub fn as_numbers(&self) -> Option<&Vec<i64>> {
        match self {
            Value::Numbers(array) => Some(&array.values),
            Value::Chars(_) => None
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Numbers(array) => write!(f, "{array}"),
            Value::Chars(array) => write!(f, "{array}")
        }
    }
}

macro_rules! dyadic_operation_env {
    ($name:ident, $numbers_operation:expr) => {
        impl Value {
            fn $name(a: Value, b: Value, env: &Environment) -> InterpretResult<Value> {
                match (a, b) {
                    (Value::Numbers(array_a), Value::Numbers(array_b)) => {
                        Array::dyadic_op_mut(array_a, array_b, $numbers_operation)
                            .map(Value::Numbers)
                            .map_err(|err| env.spanned(RuntimeError::from(err)))
                    }
                    _ => todo!()
                }
            }
        }
    };
}

dyadic_operation_env!(add, |&a, &b| a.add(b));
dyadic_operation_env!(subtract, |&a, &b| a.sub(b));
dyadic_operation_env!(multiply, |&a, &b| a.mul(b));
dyadic_operation_env!(divide, |&a, &b| a.div(b));
dyadic_operation_env!(xor, |&a, &b| a.bitxor(b));
dyadic_operation_env!(and, |&a, &b| a.bitand(b));
dyadic_operation_env!(or, |&a, &b| a.bitor(b));
dyadic_operation_env!(max, |&a, &b| a.max(b));
dyadic_operation_env!(min, |&a, &b| a.min(b));
dyadic_operation_env!(equal, |&a, &b| (a == b) as i64);
dyadic_operation_env!(not_equal, |&a, &b| (a != b) as i64);
dyadic_operation_env!(greater, |&a, &b| (a > b) as i64);
dyadic_operation_env!(greater_equal, |&a, &b| (a >= b) as i64);
dyadic_operation_env!(less, |&a, &b| (a < b) as i64);
dyadic_operation_env!(less_equal, |&a, &b| (a <= b) as i64);

macro_rules! monadic_operation {
    ($name:ident, $operation:ident) => {
        impl Value {
            fn $name(value: Value) -> InterpretResult<Value> {
                match value {
                    Value::Numbers(mut array) => {
                        array.values.$operation();
                        Ok(Value::Numbers(array))
                    },
                    Value::Chars(mut array) => {
                        array.values.$operation();
                        Ok(Value::Chars(array))
                    }
                }
            }
        }
    };
}

monadic_operation!(reverse, reverse);

impl Value {
    fn not(value: Value, env: &Environment) -> InterpretResult<Value> {
        match value {
            Value::Numbers(mut array) => {
                for value in array.values.iter_mut() {
                    *value = if *value == 0 { 1 } else { 0 }
                }
                Ok(Value::Numbers(array))
            }
            Value::Chars(_) => Err(env.make_error(RuntimeError::CannotNotCharacter))
        }
    }

    fn range(value: Value, env: &Environment) -> InterpretResult<Value> {
        match value {
            Value::Numbers(mut array) if array.shape.is_scalar() => {
                let count = array.values.pop().unwrap();

                for i in 0..count {
                    array.values.push(i)
                }

                array.shape.prepend_dimension(array.values.len());

                Ok(Value::Numbers(array))
            }
            _ => Err(env.make_error(RuntimeError::ExpectedScalarNumber))
        }
    }

    fn negate(value: Value) -> InterpretResult<Value> {
        match value {
            Value::Numbers(mut array) => {
                for value in array.values.iter_mut() {
                    *value = -*value
                }
                Ok(Value::Numbers(array))
            }
            Value::Chars(mut array) => {
                for value in array.values.iter_mut() {
                    *value = value.to_ascii_uppercase()
                }
                Ok(Value::Chars(array))
            }
        }
    }

    fn partition(a: Value, b: Value, env: &Environment) -> InterpretResult<Value> {
        match (a, b) {
            (Value::Numbers(mask), Value::Numbers(mut arr)) => {
                let mut num_rows = 0;
                let mut row_len: Option<usize> = None;
                let mut position = mask.values.len() - 1;

                for row in mask.values.split(|&v| v != 0).rev() {
                    if *row_len.get_or_insert(row.len()) != row.len() {
                        return Err(env.make_error(RuntimeError::IncompatibleArrayShapes));
                    }

                    position -= row.len();
                    num_rows += 1;
                    arr.values.remove(position);
                }

                // Fixup the dimensions
                arr.shape.replace_dimension(0, row_len.unwrap());
                arr.shape.prepend_dimension(num_rows);

                Ok(Value::Numbers(arr))
            }
            // TODO: partition chars
            _ => Err(env.make_error(RuntimeError::ExpectedNumericArray))
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
            (Value::Numbers(array_a), Value::Chars(array_b)) => {
                if !array_a.shape.is_one_dimensional() {
                    Err(env.make_error(RuntimeError::ExpectedLogicalArray))
                } else {
                    let new_rows: Vec<Vec<char>> = array_b
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
                    Ok(Value::Chars(Array::new(new_shape, new_rows.into_iter().flatten().collect())))
                }
            }
            _ => Err(env.make_error(RuntimeError::ExpectedNumericArray))
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

    pub(crate) fn execute_monadic_env(
        &mut self,
        f: fn(Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        self.push(f(a, self)?);
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
            Primitive::Not => env.execute_monadic_env(Value::not)?,
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
            Primitive::Keep => env.execute_dyadic_env(Value::keep)?,
            Primitive::Stack => {
                for value in env.stack.iter().rev() {
                    println!("{value}")
                }
            }
            Primitive::Max => env.execute_dyadic_env(Value::max)?,
            Primitive::Min => env.execute_dyadic_env(Value::min)?,
            Primitive::Range => env.execute_monadic_env(Value::range)?,
            Primitive::Partition => env.execute_dyadic_env(Value::partition)?,
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
                    Value::Chars(array) => {
                        let first_value = array.values.first().ok_or_else(|| env.make_error(RuntimeError::CannotReduceEmptyArray))?;
                        env.push(Value::Chars(Array::scalar(first_value.clone())));

                        for value in array.values.iter().skip(1) {
                            env.push(Value::Chars(Array::scalar(value.clone())));

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
                    Value::Chars(array) => {
                        env.push(initial);

                        for value in array.values {
                            env.push(Value::Chars(Array::scalar(value.clone())));

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
            Word::String(str) => {
                let value = Value::Chars(Array::linear(str.chars().collect()));
                env.push(value)
            }
            Word::Char(c) => {
                let value = Value::Chars(Array::scalar(c.clone()));
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
