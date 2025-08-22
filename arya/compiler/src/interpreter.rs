use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::parser::{parse, Node, Operator, ParseError, ParseNodeResult};
use crate::location::{Span, Spanned};
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

    fn is_scalar(&self) -> bool {
        self.dimensions.len() == 0
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.dimensions)
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.shape.dimensions.len() {
            0 => write!(f, "{}", self.values.first().unwrap()),
            1 => {
                write!(f, "[")?;
                for (i, v) in self.values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?
                    }
                    write!(f, "{}", v)?
                }
                write!(f, "]")
            },
            2 => {
                let num_columns = self.shape.dimensions.get(1).unwrap();
                let max_width = self.values.iter()
                    .map(|&value| value.checked_ilog10().unwrap_or(0) + 1)
                    .max()
                    .unwrap_or(1) as usize;

                write!(f, "[")?;
                for (i, v) in self.values.iter().enumerate() {
                    if i > 0{
                        write!(f, " ")?
                    }
                    write!(f, "{:>max_width$}", v)?;
                    if i % num_columns == num_columns - 1 && i < self.values.len() - 1 {
                        write!(f, "\n")?;
                    }
                }
                write!(f, "]")
            },
            _ => write!(f, "{:?}", self)
        }
    }
}

pub type EvaluateResult = Result<Spanned<Value>, Spanned<RuntimeError>>;
pub type InterpretResult = Result<Vec<Spanned<Value>>, Spanned<RuntimeError>>;

pub struct Interpreter<T> where T : Iterator<Item =ParseNodeResult> {
    nodes: Peekable<T>,
    stack: Vec<Spanned<Value>>,
    span: Span
}

macro_rules! binary_operation {
    ($self:ident, $operation:tt) => {
        if $self.stack.len() < 2 {
            return Err($self.spanned(RuntimeError::InvalidNumberOfArguments(2, $self.stack.len() as u8)))
        } else {
            let rhs = $self.stack.pop().unwrap();
            let lhs = $self.stack.pop().unwrap();

            if lhs.value.shape.is_scalar() {
                let lhs_value = lhs.value.values.first().unwrap();
                let summed_values: Vec<i64> = rhs.value.values.iter().map(|value| value $operation lhs_value).collect();

                Ok($self.spanned(Value::new(rhs.value.shape, summed_values)))
            } else if rhs.value.shape.is_scalar() {
                let rhs_value = rhs.value.values.first().unwrap();
                let summed_values: Vec<i64> = lhs.value.values.iter().map(|value| value $operation rhs_value).collect();

                Ok($self.spanned(Value::new(lhs.value.shape, summed_values)))
            } else if lhs.value.shape == rhs.value.shape {
                let summed_values: Vec<i64> = lhs.value.values.iter().zip(rhs.value.values)
                    .map(|(lhs_value, rhs_value)| lhs_value $operation rhs_value)
                    .collect();
                Ok($self.spanned(Value::new(lhs.value.shape, summed_values)))
            } else {
                Err($self.spanned(RuntimeError::InvalidArgumentType))
            }
        }
    };
}

impl<T> Interpreter<T> where T : Iterator<Item =ParseNodeResult> {
    pub fn new(nodes: T) -> Self {
        Self { nodes: nodes.peekable(), stack: Vec::new(), span: Span::EMPTY }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        while let Some(node) = self.nodes.next() {
            match node {
                Ok(node) => {
                    self.span = node.span.clone();
                    match self.evaluate(&node) {
                        Ok(value) => self.stack.push(value),
                        Err(error) => return Err(error)
                    }
                }
                Err(error) => {
                    self.span = error.span.clone();
                    return Err(self.spanned(RuntimeError::Parse(error.value)))
                }
            }
        }

        Ok(self.stack.clone())
    }

    fn evaluate(&mut self, node: &Spanned<Node>) -> EvaluateResult {
        match &node.value {
            Node::Integer(i) => self.integer(i),
            Node::Operation(op) => self.operator(op),
            Node::Array(elements) => self.array(elements)
        }
    }

    fn integer(&self, i: &i64) -> Result<Spanned<Value>, Spanned<RuntimeError>> {
        Ok(self.spanned(Value::scalar(i.clone())))
    }

    fn operator(&mut self, op: &Operator) -> EvaluateResult {
        match op {
            Operator::Add      => binary_operation!(self, +),
            Operator::Subtract => binary_operation!(self, -),
            Operator::Multiply => binary_operation!(self, *),
            Operator::Divide   => binary_operation!(self, /),
            Operator::Xor      => binary_operation!(self, ^),
        }
    }

    fn array(&mut self, elements: &Vec<Spanned<Node>>) -> EvaluateResult {
        let mut array_shape: Option<Shape> = None;
        let mut array_values: Vec<i64> = Vec::new();

        for spanned_element in elements {
            let spanned_value = self.evaluate(spanned_element)?;
            let value_shape = spanned_value.value.shape;
            let existing_shape = array_shape.get_or_insert(value_shape.clone());
            if *existing_shape != value_shape {
                self.span = spanned_element.span.clone();
                return Err(self.spanned(RuntimeError::DifferentArrayElementShapes))
            }

            for integer in spanned_value.value.values {
                array_values.push(integer)
            }
        }

        let mut shape = array_shape.get_or_insert(Shape::SCALAR).clone();
        shape.prepend_dimension(elements.len());
        Ok(self.spanned(Value::new(shape, array_values)))
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult {
    let nodes = parse(source);
    let mut interpreter = Interpreter::new(nodes);
    interpreter.interpret()
}
