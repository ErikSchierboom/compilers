use crate::array::{DimensionalArray, Shape};
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

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Char(char),
    Number(f64),
    String(String),
    Lambda(Vec<Word>),
    Array(Array),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Array {
    Empty,
    Char(DimensionalArray<char>),
    Number(DimensionalArray<f64>),
    String(DimensionalArray<String>),
}

impl Array {
    pub fn shape(&self) -> Shape {
        match self {
            Array::Empty => Shape::empty(),
            Array::Char(array) => array.shape.clone(),
            Array::Number(array) => array.shape.clone(),
            Array::String(array) => array.shape.clone()
        }
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Array::Empty => write!(f, "[]"),
            Array::Char(array) => write!(f, "{}", array),
            Array::Number(array) => write!(f, "{}", array),
            Array::String(array) => write!(f, "{}", array),
        }
    }
}

impl Value {
    pub fn take_char(&mut self) -> Option<char> {
        match self {
            Value::Char(c) => Some(*c),
            _ => None
        }
    }

    pub fn take_number(&mut self) -> Option<f64> {
        match self {
            Value::Number(number) => Some(*number),
            _ => None
        }
    }

    pub fn take_string(&mut self) -> Option<String> {
        match self {
            Value::String(string) => Some(*string),
            _ => None
        }
    }

    pub fn take_lambda(&mut self) -> Option<Vec<Word>> {
        match self {
            Value::Lambda(words) => Some(*words),
            _ => None
        }
    }

    fn shape(&self) -> Shape {
        match self {
            Value::Array(array) => array.shape(),
            _ => Shape::empty()
        }
    }

    fn array_value_kind(&self) -> ArrayValueKind {
        match self {
            Value::Array(array) => {
                match array {
                    Array::Empty => ArrayValueKind::Empty,
                    Array::Char(_) => ArrayValueKind::Char,
                    Array::Number(_) => ArrayValueKind::Number,
                    Array::String(_) => ArrayValueKind::String
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
            Value::Array(array) => write!(f, "{array}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayValueKind {
    Empty,
    Char,
    Number,
    String,
    Lambda,
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

            Value::Array(ArrayValueKind::Empty, _) => {
                $env.push(value);
                Ok(())
            }

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }};
}

macro_rules! dyadic_number_op {
    ($env:expr, $op:expr) => {{
        let right_val = $env.pop()?;
        let left_val = $env.pop()?;

        match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => {
                $env.push(Value::Number($op(l, r)));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Number, mut array)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number($op(scalar,val));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number($op(val, scalar));
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
                    *l = Value::Number($op(li, ri));
                }
                $env.push(Value::Array(ArrayValueKind::Number, left));
                Ok(())
            }

            _ => Err(RuntimeError::UnsupportedArgumentTypes),
        }
    }};
}

// TODO: use methods on array
macro_rules! dyadic_number_char_op {
    ($env:expr, $number_op:expr, $char_op:expr) => {{
        let right_val = $env.pop()?;
        let left_val = $env.pop()?;

        match (left_val, right_val) {
            (Value::Number(l), Value::Number(r)) => {
                $env.push(Value::Number($number_op(l, r)));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Number, mut array)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number($number_op(scalar, val));
                }
                $env.push(Value::Array(ArrayValueKind::Number, array));
                Ok(())
            }

            (Value::Array(ArrayValueKind::Number, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &val = elem.as_number().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Number($number_op(val, scalar));
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
                    *l = Value::Number($number_op(li, ri));
                }
                $env.push(Value::Array(ArrayValueKind::Number, left));
                Ok(())
            }

            (Value::Number(i), Value::Char(c)) | (Value::Char(c), Value::Number(i)) => {
                $env.push(Value::Char(($char_op(c as u8, i as u8)) as char));
                Ok(())
            }

            (Value::Number(scalar), Value::Array(ArrayValueKind::Char, mut array))
            | (Value::Array(ArrayValueKind::Char, mut array), Value::Number(scalar)) => {
                for elem in &mut array.elements {
                    let &c = elem.as_char().ok_or(RuntimeError::UnsupportedArgumentTypes)?;
                    *elem = Value::Char(($char_op(c as u8, scalar as u8)) as char);
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
                    *c = Value::Char(($char_op(ch as u8, int as u8)) as char);
                }

                $env.push(Value::Array(ArrayValueKind::Char, chars));
                Ok(())
            }

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
    Split,
}

impl Executable for Builtin {
    fn execute(&self, env: &mut Environment) -> InterpretResult {
        match self {
            Builtin::Max => dyadic_number_op!(env, f64::max),
            Builtin::Min => dyadic_number_op!(env, f64::min),
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
            Builtin::Split => {
                let delimiters = env.pop()?;
                let string = env.pop()?;

                match (string, delimiters) {
                    (Value::String(str), Value::Char(c)) => {
                        todo!()
                    }
                    (Value::String(str), Value::String(c)) => {
                        todo!()
                    }
                    (Value::String(str), Value::Array(ArrayValueKind::Char, chars)) => {
                        todo!()
                    }
                    (Value::String(str), Value::Array(ArrayValueKind::String, strings)) => {
                        todo!()
                    }
                    _ => Err(RuntimeError::UnsupportedArgumentTypes),
                }
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
                let array = if elements.is_empty() {
                    Array::Empty
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

                    let first = values.first().unwrap();

                    let mut shape = first.shape();
                    shape.prepend_dimension(values.len());

                    match first.array_value_kind() {
                        ArrayValueKind::Empty => Array::Empty,
                        ArrayValueKind::Char => Array::Char(DimensionalArray::new(shape, values.into_iter().map(|mut value| value.take_char().unwrap().clone()).collect())),
                        ArrayValueKind::Number => Array::Char(DimensionalArray::new(shape, values.into_iter().map(|value| value.take_number().unwrap().clone()).collect())),
                        ArrayValueKind::String => Array::Char(DimensionalArray::new(shape, values.into_iter().map(|value| value.take_string().unwrap().clone()).collect())),
                        ArrayValueKind::Lambda => panic!("Arrays cannot contain lambdas")
                    }
                };

                env.push(Value::Array(array))
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
                let mut val = env.pop()?;

                match val {
                    Value::Number(ref mut number) => {
                        *number = 1. - *number;
                        env.push(val);
                        Ok(())
                    }

                    Value::Array(Array::Number(ref mut array)) => {
                        for elem in &mut array.elements {
                            *elem = 1. - *elem;
                        }
                        env.push(val);
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
            DyadicOperation::Add => dyadic_number_char_op!(env, f64::add, u8::add),
            DyadicOperation::Sub => dyadic_number_char_op!(env, f64::sub, u8::sub),
            DyadicOperation::Mul => dyadic_number_char_op!(env, f64::mul, u8::mul),
            DyadicOperation::Div => dyadic_number_char_op!(env, f64::div, u8::div),
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
    use crate::parser::Word::Array;
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
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Empty, DimensionalArray::new(Shape::new(vec![0]), vec![]))]), tokens);

        let tokens = interpret("[1 2 3]");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![3]), vec![Value::Number(1.), Value::Number(2.), Value::Number(3.)]))]), tokens);
    }

    #[test]
    fn test_interpret_not() {
        let tokens = interpret("1 !");
        assert_eq!(Ok(vec![Value::Number(0.)]), tokens);

        let tokens = interpret("[0 1 2 3] !");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![4]), vec![Value::Number(1.), Value::Number(0.), Value::Number(-1.), Value::Number(-2.)]))]), tokens);

        let tokens = interpret("1.77 !");
        assert_eq!(Ok(vec![Value::Number(-0.77)]), tokens);

        let tokens = interpret("[0.5 1.0 2.0 3.3] !");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![4]), vec![Value::Number(0.5), Value::Number(0.0), Value::Number(-1.0), Value::Number(-2.3)]))]), tokens);
    }

    #[test]
    fn test_interpret_abs() {
        let tokens = interpret("1 abs");
        assert_eq!(Ok(vec![Value::Number(1.)]), tokens);

        let tokens = interpret("-3.2 abs");
        assert_eq!(Ok(vec![Value::Number(3.2)]), tokens);
    }

    #[test]
    fn test_interpret_max() {
        let tokens = interpret("1 2 max");
        assert_eq!(Ok(vec![Value::Number(2.)]), tokens);

        let tokens = interpret("3 2 max");
        assert_eq!(Ok(vec![Value::Number(3.)]), tokens);

        let tokens = interpret("[0 1 4] 2 max");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![2., Value::Number(2.), Value::Number(4.)])))]), tokens);

        let tokens = interpret("2 [0 1 4] max");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number(DimensionalArray::new(Shape::new(vec![3]), vec![2., Value::Number(2.), Value::Number(4.)])))]), tokens);
    }

    #[test]
    fn test_interpret_min() {
        let tokens = interpret("1 2 min");
        assert_eq!(Ok(vec![Value::Number(1.)]), tokens);

        let tokens = interpret("3 2 min");
        assert_eq!(Ok(vec![Value::Number(2.)]), tokens);

        let tokens = interpret("[0 1 4] 2 min");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![0., 1., 2.])))]), tokens);

        let tokens = interpret("2 [0 1 4] min");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![0., 1., 2.])))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_numbers() {
        let tokens = interpret("1 2 +");
        assert_eq!(Ok(vec![Value::Number(3.)]), tokens);

        let tokens = interpret("13.33 243.09 +");
        assert_eq!(Ok(vec![Value::Number(256.42)]), tokens);

        let tokens = interpret("1 [2 3 4] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![3]), vec![Value::Number(3.), Value::Number(4.), Value::Number(5.)]))]), tokens);

        let tokens = interpret("[5 7 9] 4 +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![3]), vec![Value::Number(9.), Value::Number(11.), Value::Number(13.)]))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] +");
        assert_eq!(Ok(vec![Value::Array(ArrayValueKind::Number, DimensionalArray::new(Shape::new(vec![3]), vec![Value::Number(13.), Value::Number(6.), Value::Number(6.)]))]), tokens);
    }

    #[test]
    fn test_interpret_addition_on_characters() {
        let tokens = interpret("'a' 2 +");
        assert_eq!(Ok(vec![Value::Char('c')]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 +");
        assert_eq!(Ok(vec![Value::Array(Array::Char(DimensionalArray::new(Shape::new(vec![3]), vec!['e', 'g', 'j'])))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_numbers() {
        let tokens = interpret("4 2 -");
        assert_eq!(Ok(vec![Value::Number(2.)]), tokens);

        let tokens = interpret("0.3 9.0 -");
        assert_eq!(Ok(vec![Value::Number(-8.7)]), tokens);

        let tokens = interpret("1 [2 3 4] -");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![-1., -2., -3.])))]), tokens);

        let tokens = interpret("[5 7 9] 4 -");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![1., 3., 5.])))]), tokens);

        let tokens = interpret("[7 4 1] [6 2 5] -");
        assert_eq!(Ok(vec![Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![3]), vec![1., 2., -4.])))]), tokens);
    }

    #[test]
    fn test_interpret_subtraction_on_characters() {
        let tokens = interpret("'d' 2 -");
        assert_eq!(Ok(vec![Value::Char('b')]), tokens);

        let tokens = interpret("['c' 'e' 'h'] 2 -");
        assert_eq!(Ok(vec![Value::Array(Array::Char(DimensionalArray::new(Shape::new(vec![3]), vec!['a', 'c', 'f'])))]), tokens);
    }

    #[test]
    fn test_interpret_dup() {
        let tokens = interpret("3 dup");
        assert_eq!(Ok(vec![Value::Number(3.), Value::Number(3.)]), tokens);

        let tokens = interpret("2 3 dup");
        assert_eq!(Ok(vec![Value::Number(2.), Value::Number(3.), Value::Number(3.)]), tokens);
    }

    #[test]
    fn test_interpret_swap() {
        let tokens = interpret("2 3 swap");
        assert_eq!(Ok(vec![Value::Number(3.), Value::Number(2.)]), tokens);

        let tokens = interpret("2 3 4 swap");
        assert_eq!(Ok(vec![Value::Number(2.), Value::Number(4.), Value::Number(3.)]), tokens);
    }

    #[test]
    fn test_interpret_split() {
        let tokens = interpret(r#""one\ntwo\nthree" '\n'"#);
        assert_eq!(Ok(vec![Value::String("one ".to_string()), Value::String("two".to_string()), Value::String("three".to_string())]), tokens);

        let tokens = interpret(r#""hi the-re"" "the" split"#);
        assert_eq!(Ok(vec![Value::String("hi ".to_string()), Value::String("-re".to_string())]), tokens);
    }
}

