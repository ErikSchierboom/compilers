use crate::array::Array;
use crate::location::{Span, Spanned};
use crate::parser::{parse, Lambda, ParseError, ParseWordResult, Primitive, Word};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    InvalidNumberOfArguments(u8, u8),
    InvalidArgumentType(String, String),
    IncompatibleShapes,
    UnknownSymbol(String),
    ExpectedArray,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::InvalidNumberOfArguments(expected, actual) => {
                write!(f, "Expected {expected} arguments, got {actual}")
            }
            RuntimeError::IncompatibleShapes => write!(f, "Incompatible shapes"),
            RuntimeError::UnknownSymbol(name) => write!(f, "Unknown identifier: {name}"),
            RuntimeError::InvalidArgumentType(expected, actual) => write!(f, "Invalid argument. Expected: {expected}, actual: {actual}"),
            RuntimeError::ExpectedArray => write!(f, "Expected array argument")
        }
    }
}

impl Error for RuntimeError {}

#[derive(Clone, Debug)]
pub enum Value {
    Array(Array),
    Lambda(Lambda),
}

impl Value {
    fn as_array(&self) -> Option<&Array> {
        match self {
            Value::Array(array) => Some(array),
            _ => None
        }
    }

    fn as_lambda(&self) -> Option<&Lambda> {
        match self {
            Value::Lambda(lambda) => Some(lambda),
            _ => None
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(array) => write!(f, "{array}"),
            Value::Lambda(lambda) => write!(f, "{lambda}"),
        }
    }
}

type EvaluateResult<T = ()> = InterpretResult<T>;
pub type InterpretResult<T> = Result<T, Spanned<RuntimeError>>;

pub struct Interpreter<T>
where
    T: Iterator<Item=ParseWordResult>,
{
    words: Peekable<T>,
    stack: Vec<Value>,
    span: Span,
}

impl<T> Interpreter<T>
where
    T: Iterator<Item=ParseWordResult>,
{
    pub fn new(nodes: T) -> Self {
        Self {
            words: nodes.peekable(),
            stack: Vec::new(),
            span: Span::EMPTY,
        }
    }

    pub fn interpret(&mut self) -> InterpretResult<Vec<Value>> {
        while let Some(node) = self.next() {
            match node {
                Ok(node) => self.evaluate_word(node)?,
                Err(error) => {
                    self.span = error.span.clone();
                    return self.make_error(RuntimeError::Parse(error.value));
                }
            }
        }

        Ok(self.stack.clone())
    }

    fn next(&mut self) -> Option<ParseWordResult> {
        self.words.next()
    }

    fn evaluate_word(&mut self, word: Spanned<Word>) -> EvaluateResult {
        self.span = word.span.clone();

        match word.value {
            Word::Array(array) => self.array(array),
            Word::Primitive(primitive) => self.primitive(primitive),
            Word::Lambda(lambda) => self.lambda(lambda),
        }
    }

    fn evaluate_words(&mut self, words: Vec<Spanned<Word>>) -> EvaluateResult {
        for word in words {
            self.evaluate_word(word)?
        }
        Ok(())
    }

    fn lambda(&mut self, lambda: Lambda) -> EvaluateResult {
        self.push(Value::Lambda(lambda));
        Ok(())
    }

    fn primitive(&mut self, primitive: Primitive) -> EvaluateResult {
        match primitive {
            Primitive::Add => self.binary_array_operation(|l, r| l + r),
            Primitive::Subtract => self.binary_array_operation(|l, r| l - r),
            Primitive::Multiply => self.binary_array_operation(|l, r| l * r),
            Primitive::Divide => self.binary_array_operation(|l, r| l / r),
            Primitive::And => self.binary_array_operation(|l, r| l & r),
            Primitive::Or => self.binary_array_operation(|l, r| l | r),
            Primitive::Xor => self.binary_array_operation(|l, r| l ^ r),
            Primitive::Equal => self.binary_array_operation(|l, r| (l == r) as i64),
            Primitive::NotEqual => self.binary_array_operation(|l, r| (l != r) as i64),
            Primitive::Greater => self.binary_array_operation(|l, r| (l > r) as i64),
            Primitive::GreaterEqual => self.binary_array_operation(|l, r| (l >= r) as i64),
            Primitive::Less => self.binary_array_operation(|l, r| (l < r) as i64),
            Primitive::LessEqual => self.binary_array_operation(|l, r| (l <= r) as i64),
            Primitive::Not => self.unary_array_operation(|value| !value),
            Primitive::Negate => self.unary_array_operation(|value| -value),
            Primitive::Dup => {
                self.unary_operation(|value| vec![value.clone(), value.clone()])
            }
            Primitive::Drop => self.unary_operation(|_| vec![]),
            Primitive::Swap => {
                self.binary_operation(|lhs, rhs| vec![rhs.clone(), lhs.clone()])
            }
            Primitive::Over => {
                self.binary_operation(|lhs, rhs| vec![lhs.clone(), rhs.clone(), lhs.clone()])
            }
            Primitive::Reduce => {
                todo!()
                // self.binary_stack_operation(|lhs, rhs| {
                //     // TODO: error handling of stack values
                //     let func = rhs.as_function().unwrap().to_owned();
                //     if func.signature().num_inputs != 2 {}
                //
                //     let array = lhs.as_array().unwrap();
                //
                //     let reduced = array.rows().reduce(|acc, e| {
                //         self.stack.push(Value::Array(Array::new(Shape::SCALAR, acc.clone().to_vec())));
                //         self.stack.push(Value::Array(Array::new(Shape::SCALAR, e.clone().to_vec())));
                //         self.function(func.clone());
                //         let arr = self.pop();
                //         let a = arr.unwrap().as_array().unwrap().clone();
                //         a.values.as_slice()
                //     });
                //
                //     // TODO: error handling?
                //     vec![Value::Array(Array::new(array.shape.clone(), reduced.unwrap().to_vec()))]
                // });
                //
                // Ok(())
            }
        }
    }

    fn array(&mut self, array: Array) -> EvaluateResult {
        self.push(Value::Array(array));
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }

    fn binary_array_operation(&mut self, operation: impl Fn(&i64, &i64) -> i64) -> EvaluateResult {
        self.verify_stack_size(2)?;

        // TODO: convert to array
        // TODO: error handling
        let rhs = self.pop().unwrap();
        let lhs = self.pop().ok_or_else(|| return self.spanned(RuntimeError::ExpectedArray));

        todo!("binary");
        Ok(())
    }

    fn unary_array_operation(&mut self, operation: impl Fn(&i64) -> i64) -> EvaluateResult {
        self.verify_stack_size(1)?;

        // TODO: convert to array
        // TODO: error handling
        let operand = self.pop().unwrap();
        todo!("unary");
        Ok(())
    }

    fn binary_operation(
        &mut self,
        operation: impl Fn(&Value, &Value) -> Vec<Value>,
    ) -> EvaluateResult {
        self.verify_stack_size(2)?;

        let rhs = self.pop().unwrap();
        let lhs = self.pop().unwrap();
        for value in operation(&lhs, &rhs) {
            self.push(value)
        }

        Ok(())
    }

    fn unary_operation(
        &mut self,
        operation: impl Fn(&Value) -> Vec<Value>,
    ) -> EvaluateResult {
        self.verify_stack_size(1)?;

        let operand = self.pop().unwrap();
        for value in operation(&operand) {
            self.push(value)
        }

        Ok(())
    }

    fn verify_stack_size(&mut self, expected: u8) -> Result<(), Spanned<RuntimeError>> {
        let actual = self.stack.len() as u8;
        if actual < expected {
            Err(self.spanned(RuntimeError::InvalidNumberOfArguments(
                expected,
                self.stack.len() as u8,
            )))
        } else {
            Ok(())
        }
    }

    fn make_error<V>(&mut self, error: RuntimeError) -> InterpretResult<V> {
        Err(self.spanned(error))
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult<Vec<Value>> {
    let words = parse(source);
    let mut interpreter = Interpreter::new(words);
    interpreter.interpret()
}
