use crate::array::{Array, Shape};
use crate::parser::{parse, DyadicOperation, MonadicOperation, NiladicOperation, ParseError, ParseResult, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    UnsupportedArgumentTypes, // TODO: store allows argument types
    IncompatibleArrayShapes,
    IncompatibleArrayValues,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::UnsupportedArgumentTypes => write!(f, "Unsupported argument types"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
            RuntimeError::IncompatibleArrayValues => write!(f, "Incompatible array values"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArrayValueKind {
    Empty,
    Char,
    Float,
    Integer,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Char(char),
    Float(f64),
    Integer(i64),
    String(String),
    Lambda(Vec<Word>),
    Array(ArrayValueKind, Array<Value>),
}

impl Value {
    pub fn as_char(&self) -> Option<&char> {
        match self {
            Value::Char(c) => Some(c),
            _ => None
        }
    }

    pub fn as_float(&self) -> Option<&f64> {
        match self {
            Value::Float(float) => Some(float),
            _ => None
        }
    }

    pub fn as_integer(&self) -> Option<&i64> {
        match self {
            Value::Integer(int) => Some(int),
            _ => None
        }
    }

    pub fn as_string(&self) -> Option<&String> {
        match self {
            Value::String(string) => Some(string),
            _ => None
        }
    }

    fn shape(&self) -> Shape {
        match self {
            Value::Array(_, array) => array.shape.clone(),
            _ => Shape::empty()
        }
    }

    fn array_value_kind(&self) -> ArrayValueKind {
        match self {
            Value::Array(array_value_kind, array) => {
                if array.elements.is_empty() {
                    ArrayValueKind::Empty
                } else {
                    array_value_kind.clone()
                }
            }
            Value::Char(_) => ArrayValueKind::Char,
            Value::Float(_) => ArrayValueKind::Float,
            Value::Integer(_) => ArrayValueKind::Integer,
            Value::String(_) => ArrayValueKind::String,
            Value::Lambda(_) => panic!("Arrays should not contain lambdas")
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{c}"),
            Value::Float(float) => write!(f, "{float}"),
            Value::Integer(int) => write!(f, "{int}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Lambda(words) => write!(f, "({})", words.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
            Value::Array(_, array) => write!(f, "{array}"),
        }
    }
}

pub struct Environment {
    stack: Vec<Value>,
}

impl Environment {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> InterpretResult<Value> {
        self.stack.pop().ok_or_else(|| RuntimeError::EmptyStack)
    }

    fn pop_n(&mut self, n: usize) -> InterpretResult<Vec<Value>> {
        if n <= self.stack.len() {
            Ok(self.stack.drain((self.stack.len() - n)..).collect())
        } else {
            Err(RuntimeError::EmptyStack)
        }
    }

    pub(crate) fn execute_monadic(
        &mut self,
        f: fn(Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        self.push(f(a, self)?);
        Ok(())
    }

    pub(crate) fn execute_dyadic(
        &mut self,
        f: fn(Value, Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(f(a, b, self)?);
        Ok(())
    }
}

pub trait Executable {
    fn execute(&self, env: &mut Environment) -> InterpretResult;
}

impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Integer(int) => env.push(Value::Integer(int.clone())),
            Word::Float(float) => env.push(Value::Float(float.clone())),
            Word::String(string) => env.push(Value::String(string.clone())),
            Word::Char(c) => env.push(Value::Char(c.clone())),
            Word::Array(elements) => {
                let (array_value_kind, array) = if elements.is_empty() {
                    (ArrayValueKind::Empty, Array::empty())
                } else {
                    for element in elements {
                        element.value.execute(env)?;
                    }

                    let values = env.pop_n(elements.len())?;

                    for window in values.windows(2) {
                        if window[0].shape() != window[1].shape() {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        if window[0].array_value_kind() != window[1].array_value_kind() {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }
                    }

                    let mut shape = values.first().unwrap().shape();
                    shape.prepend_dimension(values.len());

                    let array_value_kind = values.iter().find_map(|value| {
                        let kind = value.array_value_kind();
                        if kind == ArrayValueKind::Empty { None } else { Some(kind) }
                    });
                    (array_value_kind.unwrap_or(ArrayValueKind::Empty), Array::new(shape, values))
                };

                env.push(Value::Array(array_value_kind, array))
            }
            Word::Lambda(words) => env.push(Value::Lambda(words.iter().map(|word| word.value.clone()).collect())),
            Word::Identifier(identifier) => {
                todo!()
            }
            Word::Niladic(niladic_op) => niladic_op.execute(env)?,
            Word::Monadic(monadic_op) => monadic_op.execute(env)?,
            Word::Dyadic(dyadic_op) => dyadic_op.execute(env)?,
        }

        Ok(())
    }
}

impl Executable for NiladicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            NiladicOperation::Stack => {
                for value in env.stack.iter().rev() {
                    println!("{value}")
                }
                Ok(())
            }
        }
    }
}

impl Executable for MonadicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            MonadicOperation::Not => {
                todo!("implement not")
            }
        }
    }
}

impl Executable for DyadicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            DyadicOperation::Add => {
                let right_val = env.pop()?;
                let left_val = env.pop()?;

                match (left_val, right_val) {
                    (Value::Integer(left), Value::Integer(right)) => {
                        env.push(Value::Integer(left + right));
                        Ok(())
                    }
                    (Value::Integer(scalar), Value::Array(ArrayValueKind::Integer, mut array)) |
                    (Value::Array(ArrayValueKind::Integer, mut array), Value::Integer(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_int = right_element.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Integer(right_int + scalar)
                        }

                        env.push(Value::Array(ArrayValueKind::Integer, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Integer, mut left), Value::Array(ArrayValueKind::Integer, right)) => {
                        if left.shape != right.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in left.elements.iter_mut().zip(right.elements.iter()) {
                            let left_int = l.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let right_int = r.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Integer(left_int + right_int);
                        }

                        env.push(Value::Array(ArrayValueKind::Integer, left));
                        Ok(())
                    }
                    (Value::Float(left), Value::Float(right)) => {
                        env.push(Value::Float(left + right));
                        Ok(())
                    }
                    (Value::Float(scalar), Value::Array(ArrayValueKind::Float, mut array)) |
                    (Value::Array(ArrayValueKind::Float, mut array), Value::Float(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_int = right_element.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Float(right_int + scalar)
                        }

                        env.push(Value::Array(ArrayValueKind::Float, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Float, mut left), Value::Array(ArrayValueKind::Float, right)) => {
                        if left.shape != right.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in left.elements.iter_mut().zip(right.elements.iter()) {
                            let left_int = l.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let right_int = r.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Float(left_int + right_int);
                        }

                        env.push(Value::Array(ArrayValueKind::Float, left));
                        Ok(())
                    }
                    (Value::Integer(i), Value::Char(c)) |
                    (Value::Char(c), Value::Integer(i)) => {
                        env.push(Value::Char((c as u8 + i as u8) as char));
                        Ok(())
                    }
                    (Value::Integer(scalar), Value::Array(ArrayValueKind::Char, mut array)) |
                    (Value::Array(ArrayValueKind::Char, mut array), Value::Integer(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_char = right_element.as_char().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Char((scalar as u8 + *right_char as u8) as char)
                        }

                        env.push(Value::Array(ArrayValueKind::Char, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Integer, integers), Value::Array(ArrayValueKind::Char, mut chars)) |
                    (Value::Array(ArrayValueKind::Char, mut chars), Value::Array(ArrayValueKind::Integer, integers)) => {
                        if chars.shape != integers.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in chars.elements.iter_mut().zip(integers.elements.iter()) {
                            let char = l.as_char().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let int = r.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Char((*char as u8 + *int as u8) as char)
                        }

                        env.push(Value::Array(ArrayValueKind::Char, chars));
                        Ok(())
                    }
                    _ => Err(RuntimeError::UnsupportedArgumentTypes)
                }
            }
            DyadicOperation::Sub => {
                let right_val = env.pop()?;
                let left_val = env.pop()?;

                match (left_val, right_val) {
                    (Value::Integer(left), Value::Integer(right)) => {
                        env.push(Value::Integer(left - right));
                        Ok(())
                    }
                    (Value::Integer(scalar), Value::Array(ArrayValueKind::Integer, mut array)) |
                    (Value::Array(ArrayValueKind::Integer, mut array), Value::Integer(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_int = right_element.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Integer(right_int - scalar)
                        }

                        env.push(Value::Array(ArrayValueKind::Integer, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Integer, mut left), Value::Array(ArrayValueKind::Integer, right)) => {
                        if left.shape != right.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in left.elements.iter_mut().zip(right.elements.iter()) {
                            let left_int = l.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let right_int = r.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Integer(left_int - right_int);
                        }

                        env.push(Value::Array(ArrayValueKind::Integer, left));
                        Ok(())
                    }
                    (Value::Float(left), Value::Float(right)) => {
                        env.push(Value::Float(left - right));
                        Ok(())
                    }
                    (Value::Float(scalar), Value::Array(ArrayValueKind::Float, mut array)) |
                    (Value::Array(ArrayValueKind::Float, mut array), Value::Float(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_int = right_element.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Float(right_int - scalar)
                        }

                        env.push(Value::Array(ArrayValueKind::Float, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Float, mut left), Value::Array(ArrayValueKind::Float, right)) => {
                        if left.shape != right.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in left.elements.iter_mut().zip(right.elements.iter()) {
                            let left_int = l.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let right_int = r.as_float().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Float(left_int - right_int);
                        }

                        env.push(Value::Array(ArrayValueKind::Float, left));
                        Ok(())
                    }
                    (Value::Integer(i), Value::Char(c)) |
                    (Value::Char(c), Value::Integer(i)) => {
                        env.push(Value::Char((c as u8 - i as u8) as char));
                        Ok(())
                    }
                    (Value::Integer(scalar), Value::Array(ArrayValueKind::Char, mut array)) |
                    (Value::Array(ArrayValueKind::Char, mut array), Value::Integer(scalar)) => {
                        for right_element in array.elements.iter_mut() {
                            let right_char = right_element.as_char().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *right_element = Value::Char((scalar as u8 - *right_char as u8) as char)
                        }

                        env.push(Value::Array(ArrayValueKind::Char, array));
                        Ok(())
                    }
                    (Value::Array(ArrayValueKind::Integer, integers), Value::Array(ArrayValueKind::Char, mut chars)) |
                    (Value::Array(ArrayValueKind::Char, mut chars), Value::Array(ArrayValueKind::Integer, integers)) => {
                        if chars.shape != integers.shape {
                            return Err(RuntimeError::IncompatibleArrayShapes);
                        }

                        for (l, r) in chars.elements.iter_mut().zip(integers.elements.iter()) {
                            let char = l.as_char().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            let int = r.as_integer().ok_or_else(|| RuntimeError::UnsupportedArgumentTypes)?;
                            *l = Value::Char((*char as u8 - *int as u8) as char)
                        }

                        env.push(Value::Array(ArrayValueKind::Char, chars));
                        Ok(())
                    }
                    _ => Err(RuntimeError::UnsupportedArgumentTypes)
                }
            }
            DyadicOperation::Mul => todo!(),
            DyadicOperation::Div => todo!(),
            DyadicOperation::Equal => todo!(),
            DyadicOperation::NotEqual => todo!(),
            DyadicOperation::Greater => todo!(),
            DyadicOperation::GreaterEqual => todo!(),
            DyadicOperation::Less => todo!(),
            DyadicOperation::LessEqual => todo!(),
            DyadicOperation::Stack => todo!(),
        }
    }
}

pub type InterpretResult<T = ()> = Result<T, RuntimeError>;

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
                Err(error) => return Err(RuntimeError::Parse(error.value))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interpret_scalars() {
        let tokens = interpret("19");
        assert_eq!(Ok(vec![Value::Integer(19)]), tokens);

        let tokens = interpret("5.32");
        assert_eq!(Ok(vec![Value::Float(5.32)]), tokens);

        let tokens = interpret("'a'");
        assert_eq!(Ok(vec![Value::Char('a')]), tokens);

        let tokens = interpret(r#""hi there""#);
        assert_eq!(Ok(vec![Value::String("hi there".to_string())]), tokens);
    }

    #[test]
    fn test_interpret_arrays() {
        let tokens = interpret("[]");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Empty, Array::new(Shape::new(vec![0]), vec![]))]), tokens);

        let tokens = interpret("[1 2 3]");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_integers() {
        let tokens = interpret("1 2 +");
        assert_eq!(Ok(vec![Value::Integer(3)]), tokens);

        let tokens = interpret("1 [2 3 4] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(3), Value::Integer(4), Value::Integer(5)]))]), tokens);

        let tokens = interpret("[5 7 9] 4 +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(9), Value::Integer(11), Value::Integer(13)]))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(13), Value::Integer(6), Value::Integer(6)]))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_characters() {
        let tokens = interpret("'a' 2 +");
        assert_eq!(Ok(vec![Value::Char('c')]), tokens);

        let tokens = interpret("3 'b' +");
        assert_eq!(Ok(vec![Value::Char('e')]), tokens);

        let tokens = interpret("4 ['f' 'e' 'r'] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('j'), Value::Char('i'), Value::Char('v')]))]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('e'), Value::Char('g'), Value::Char('j')]))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_integers() {
        let tokens = interpret("4 2 -");
        assert_eq!(Ok(vec![Value::Integer(2)]), tokens);

        let tokens = interpret("1 [2 3 4] -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]))]), tokens);

        let tokens = interpret("[5 7 9] 4 -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(1), Value::Integer(3), Value::Integer(5)]))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Integer, Array::new(Shape::new(vec![3]), vec![Value::Integer(1), Value::Integer(2), Value::Integer(-4)]))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_characters() {
        let tokens = interpret("'d' 2 -");
        assert_eq!(Ok(vec![Value::Char('b')]), tokens);

        let tokens = interpret("3 'f' -");
        assert_eq!(Ok(vec![Value::Char('c')]), tokens);

        let tokens = interpret("1 ['f' 'e' 'r'] -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('e'), Value::Char('f'), Value::Char('q')]))]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('a'), Value::Char('c'), Value::Char('f')]))]), tokens);
    }
}

