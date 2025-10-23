use crate::array::{Array, Shape};
use crate::parser::{parse, DyadicOperation, MonadicOperation, NiladicOperation, ParseError, ParseResult, Word};
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    Parse(ParseError),
    EmptyStack,
    UnsupportedArgumentTypes, // TODO: store allows argument types
    IncompatibleArrayShapes,
    IncompatibleArrayValues,
    UnknownIdentifier(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::EmptyStack => write!(f, "Missing argument"),
            RuntimeError::UnsupportedArgumentTypes => write!(f, "Unsupported argument types"),
            RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
            RuntimeError::IncompatibleArrayValues => write!(f, "Incompatible array values"),
            RuntimeError::UnknownIdentifier(identifier) => write!(f, "Unknown identifier '{identifier}'"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ArrayValueKind {
    Empty,
    Char,
    Number,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Char(char),
    Number(f64),
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

    pub fn as_number(&self) -> Option<&f64> {
        match self {
            Value::Number(number) => Some(number),
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
            Value::Number(_) => ArrayValueKind::Number,
            Value::String(_) => ArrayValueKind::String,
            Value::Lambda(_) => panic!("Arrays should not contain lambdas")
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{c}"),
            Value::Number(int) => write!(f, "{int}"),
            Value::String(string) => write!(f, "{string}"),
            Value::Lambda(words) => write!(f, "({})", words.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
            Value::Array(_, array) => write!(f, "{array}"),
        }
    }
}

#[derive(Clone)]
pub enum Binding {
    Builtin(Builtin)
}

pub struct Environment {
    stack: Vec<Value>,
    bindings: HashMap<String, Binding>,
}

impl Environment {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            bindings: HashMap::from([
                ("max".to_string(), Binding::Builtin(Builtin::Max)),
                ("min".to_string(), Binding::Builtin(Builtin::Min)),
                ("abs".to_string(), Binding::Builtin(Builtin::Abs)),
                ("dup".to_string(), Binding::Builtin(Builtin::Dup)),
                ("swap".to_string(), Binding::Builtin(Builtin::Swap)),
            ]),
        }
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

    pub fn execute_monadic(
        &mut self,
        f: fn(Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        self.push(f(a, self)?);
        Ok(())
    }

    pub fn execute_dyadic(
        &mut self,
        f: fn(Value, Value, &Self) -> InterpretResult<Value>,
    ) -> InterpretResult {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(f(a, b, self)?);
        Ok(())
    }

    pub fn get(&self, identifier: &String) -> InterpretResult<&Binding> {
        self.bindings.get(identifier).ok_or_else(|| RuntimeError::UnknownIdentifier(identifier.clone()))
    }
}

// TODO: convert op to method
// TODO: use methods on array
// TODO: create overload without characters
macro_rules! dyadic_number_char_op {
    ($env:expr, $op:ident) => {{
        let right_val = $env.pop()?;
        let left_val = $env.pop()?;

        match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => {
                $env.push(Value::Number(l.$op(r)));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Number, mut array)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number(scalar.$op(val));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number(val.$op(scalar));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut left), Value::Array(ArrayValueKind::Number, right)) => {
                if left.shape != right.shape {
                    return Err(RuntimeError::IncompatibleArrayShapes);
                }
                for (l, r) in left.elements.iter_mut().zip(&right.elements) {
                    let &li = l.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    let &ri = r.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *l = Value::Number(li.$op(ri));
                }
                $env.push(Value::Array(ArrayValueKind::Number, left));
                Ok(())
            }

            (Value::Number(i), Value::Char(c)) | (Value::Char(c), Value::Number(i)) => {
                $env.push(Value::Char(((c as u8).$op(i as u8)) as char));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Char, mut array))
            | (Value::Array(ArrayValueKind::Char, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &c = elem.as_char().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Char(((c as u8).$op(scalar as u8)) as char);
                }
                $env.push(Value::Array(ArrayValueKind::Char, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Char, mut chars), Value::Array(ArrayValueKind::Number, ints)) => {
                if chars.shape != ints.shape {
                    return Err(RuntimeError::IncompatibleArrayShapes);
                }

                for (c, i) in chars.elements.iter_mut().zip(&ints.elements) {
                    let &ch = c.as_char().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    let &int = i.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *c = Value::Char(((ch as u8).$op(int as u8)) as char);
                }

                $env.push(Value::Array(ArrayValueKind::Char, chars));
                Ok(())
            }

            // TODO: support empty arrays

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }};
}

macro_rules! dyadic_number_op {
    ($env:expr, $op:ident) => {{
        let right_val = $env.pop()?;
        let left_val = $env.pop()?;

        match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => {
                $env.push(Value::Number(l.$op(r)));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Number, mut array)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number(scalar.$op(val));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number(val.$op(scalar));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut left), Value::Array(ArrayValueKind::Number, right)) => {
                if left.shape != right.shape {
                    return Err(RuntimeError::IncompatibleArrayShapes);
                }
                for (l, r) in left.elements.iter_mut().zip(&right.elements) {
                    let &li = l.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    let &ri = r.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *l = Value::Number(li.$op(ri));
                }
                $env.push(Value::Array(ArrayValueKind::Number, left));
                Ok(())
            }

            // TODO: support empty arrays

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }};
}

#[derive(Clone)]
pub enum Builtin {
    Max,
    Min,
    Abs,
    Dup,
    Swap,
}

macro_rules! monadic_number_op {
    ($env:expr, $op:expr) => {{
        let value = $env.pop()?;

        match value {
            Value::Number(number) => {
                $env.push(Value::Number($op(number)));
                Ok(())
            }

            Value::Array(ArrayValueKind::Number, mut array) => {
                for element in &mut array.elements {
                    let &element_value = element.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *element = Value::Number($op(element_value));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            Value::Array(ArrayValueKind::Empty, array) => {
                $env.push(Value::Array(ArrayValueKind::Empty, array));
                Ok(())
            }

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }};
}

impl Executable for Builtin {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Builtin::Max => dyadic_number_op!(env, max),
            Builtin::Min => dyadic_number_op!(env, min),
            Builtin::Abs => monadic_number_op!(env, f64::abs),
            Builtin::Dup => {
                let value = env.pop()?;
                env.push(value.clone());
                env.push(value);
                Ok(())
            }
            Builtin::Swap => {
                let right = env.pop()?;
                let left = env.pop()?;
                env.push(right);
                env.push(left);
                Ok(())
            }
        }
    }
}

pub trait Executable {
    fn execute(&self, env: &mut Environment) -> InterpretResult;
}


impl Executable for Word {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Word::Number(number) => env.push(Value::Number(number.clone())),
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
                match env.get(identifier)?.clone() {
                    Binding::Builtin(builtin) => builtin.execute(env)?,
                }
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
                let val = env.pop()?;

                match val {
                    Value::Number(number) => {
                        env.push(Value::Number(1. - number));
                        Ok(())
                    }

                    Value::Array(ArrayValueKind::Number, mut array) => {
                        for elem in &mut array.elements {
                            let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                            *elem = Value::Number(1. - val);
                        }
                        env.push(Value::Array(ArrayValueKind::Number, array));
                        Ok(())
                    }

                    _ => Err(RuntimeError::UnsupportedArgumentTypes),
                }
            }
        }
    }
}

impl Executable for DyadicOperation {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            DyadicOperation::Add => dyadic_number_char_op!(env, add),
            DyadicOperation::Sub => dyadic_number_char_op!(env, sub),
            DyadicOperation::Mul => dyadic_number_char_op!(env, mul),
            DyadicOperation::Div => dyadic_number_char_op!(env, div),
            DyadicOperation::Equal => todo!(),
            DyadicOperation::NotEqual => todo!(),
            DyadicOperation::Greater => todo!(),
            DyadicOperation::GreaterEqual => todo!(),
            DyadicOperation::Less => todo!(),
            DyadicOperation::LessEqual => todo!(),
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
        assert_eq!(Ok(vec![Value::Number(19.)]), tokens);

        let tokens = interpret("5.32");
        assert_eq!(Ok(vec![Value::Number(5.32)]), tokens);

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
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(1.), Value::Number(2.), Value::Number(3.)]))]), tokens);
    }

    #[test]
    fn test_interpret_not() {
        let tokens = interpret("1 !");
        assert_eq!(Ok(vec![Value::Number(0.)]), tokens);

        let tokens = interpret("[0 1 2 3] !");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![4]), vec![Value::Number(1.), Value::Number(0.), Value::Number(-1.), Value::Number(-2.)]))]), tokens);

        let tokens = interpret("1.77 !");
        assert_eq!(Ok(vec![Value::Number(-0.77)]), tokens);

        let tokens = interpret("[0.5 1.0 2.0 3.3] !");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![4]), vec![Value::Number(0.5), Value::Number(0.0), Value::Number(-1.0), Value::Number(-2.3)]))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_numbers() {
        let tokens = interpret("1 2 +");
        assert_eq!(Ok(vec![Value::Number(3.)]), tokens);

        let tokens = interpret("13.33 243.09 +");
        assert_eq!(Ok(vec![Value::Number(256.42)]), tokens);

        let tokens = interpret("1 [2 3 4] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(3.), Value::Number(4.), Value::Number(5.)]))]), tokens);

        let tokens = interpret("[5 7 9] 4 +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(9.), Value::Number(11.), Value::Number(13.)]))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(13.), Value::Number(6.), Value::Number(6.)]))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_characters() {
        let tokens = interpret("'a' 2 +");
        assert_eq!(Ok(vec![Value::Char('c')]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('e'), Value::Char('g'), Value::Char('j')]))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_numbers() {
        let tokens = interpret("4 2 -");
        assert_eq!(Ok(vec![Value::Number(2.)]), tokens);

        let tokens = interpret("0.3 9.0 -");
        assert_eq!(Ok(vec![Value::Number(-8.7)]), tokens);

        let tokens = interpret("1 [2 3 4] -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(-1.), Value::Number(-2.), Value::Number(-3.)]))]), tokens);

        let tokens = interpret("[5 7 9] 4 -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(1.), Value::Number(3.), Value::Number(5.)]))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, Array::new(Shape::new(vec![3]), vec![Value::Number(1.), Value::Number(2.), Value::Number(-4.)]))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_characters() {
        let tokens = interpret("'d' 2 -");
        assert_eq!(Ok(vec![Value::Char('b')]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 -");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Char, Array::new(Shape::new(vec![3]), vec![Value::Char('a'), Value::Char('c'), Value::Char('f')]))]), tokens);
    }
}

