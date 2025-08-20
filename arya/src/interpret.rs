use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::parse::{parse, Node, ParseError, ParseNodeResult};
use crate::location::{Spanned};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    InvalidNumberOfArguments(u8, u8),
    InvalidArgumentType,
    DifferentArrayElementShapes,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::InvalidNumberOfArguments(expected, actual) => write!(f, "Expected {expected} arguments, got {actual}"),
            RuntimeError::InvalidArgumentType => write!(f, "Invalid argument type"),
            RuntimeError::DifferentArrayElementShapes => write!(f, "Not all rows in the array have the same shape")
        }
    }
}

impl Error for RuntimeError {}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Shape {
    dimensions: Vec<usize>
}

impl Shape {
    pub const SCALAR: Self = Self { dimensions: vec![] };

    pub fn prepend_dimension(&mut self, size: usize) {
        self.dimensions.insert(0, size)
    }
}

#[derive(Clone, Debug)]
pub struct Value {
    pub shape: Shape,
    pub values: Vec<i64>
}

impl Value {
    pub fn new(shape: Shape, elements: Vec<i64>) -> Self {
        Self { shape, values: elements }
    }

    pub fn scalar(element: i64) -> Self {
        Self::new(Shape::SCALAR, vec!(element))
    }
}

pub type EvaluateResult = Result<Spanned<Value>, Spanned<RuntimeError>>;
pub type InterpretResult = Result<Vec<Spanned<Value>>, Spanned<RuntimeError>>;

pub struct Interpreter<T> where T : Iterator<Item =ParseNodeResult> {
    nodes: Peekable<T>,
    stack: Vec<Spanned<Value>>
}

impl<T> Interpreter<T> where T : Iterator<Item =ParseNodeResult> {
    pub fn new(nodes: T) -> Self {
        Self { nodes: nodes.peekable(), stack: Vec::new() }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        while let Some(node) = self.nodes.next() {
            match node {
                Ok(node) => {
                    match self.evaluate(&node) {
                        Ok(value) => self.stack.push(value),
                        Err(error) => return Err(error)
                    }
                }
                Err(error) => return Err(Spanned::new(RuntimeError::Parse(error.value), error.span))
            }
        }

        Ok(self.stack.clone())
    }

    fn evaluate(&mut self, node: &Spanned<Node>) -> EvaluateResult {
        match &node.value {
            Node::Integer(i) => Ok(Spanned::new(Value::scalar(i.clone()), node.span.clone())),
            Node::Operator(_) => todo!(),
            Node::Array(elements) => {
                let mut array_shape: Option<Shape> = None;
                let mut array_values: Vec<i64> = Vec::new();

                for element in elements {
                    let element_value = self.evaluate(element)?;
                    let element_shape = element_value.value.shape;
                    let existing_shape = array_shape.get_or_insert(element_shape.clone());
                    if *existing_shape != element_shape {
                        return Err(Spanned::new(RuntimeError::DifferentArrayElementShapes, element.span.clone()))
                    }

                    for integer in element_value.value.values {
                        array_values.push(integer)
                    }
                }

                let mut shape= array_shape.get_or_insert(Shape::SCALAR).clone();
                shape.prepend_dimension(elements.len());
                Ok(Spanned::new(Value::new(shape.clone(), array_values), node.span.clone()))
            }
        }
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult {
    let nodes = parse(source);
    let mut interpreter = Interpreter::new(nodes);
    interpreter.interpret()
}
