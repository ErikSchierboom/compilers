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
            Operator::Add          => self.binary_operation(|l,r| l + r),
            Operator::Subtract     => self.binary_operation(|l,r| l - r),
            Operator::Multiply     => self.binary_operation(|l,r| l * r),
            Operator::Divide       => self.binary_operation(|l,r| l / r),
            Operator::And          => self.binary_operation(|l,r| l & r),
            Operator::Or           => self.binary_operation(|l,r| l | r),
            Operator::Xor          => self.binary_operation(|l,r| l ^ r),
            Operator::Equal        => self.binary_operation(|l,r| (l == r) as i64),
            Operator::NotEqual     => self.binary_operation(|l,r| (l != r) as i64),
            Operator::Greater      => self.binary_operation(|l,r| (l >  r) as i64),
            Operator::GreaterEqual => self.binary_operation(|l,r| (l >= r) as i64),
            Operator::Less         => self.binary_operation(|l,r| (l <  r) as i64),
            Operator::LessEqual    => self.binary_operation(|l,r| (l <= r) as i64),
            Operator::Not          => self.unary_operation(|value| !value),
            Operator::Negate       => self.unary_operation(|value| -value),
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

    fn binary_operation(&mut self, operation: impl Fn(&i64, &i64) -> i64) -> EvaluateResult {
        if self.stack.len() < 2 {
            return Err(self.spanned(RuntimeError::InvalidNumberOfArguments(2, self.stack.len() as u8)))
        }

        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        if lhs.value.shape.is_scalar() {
            let lhs_value = lhs.value.values.first().unwrap();
            let transformed_values: Vec<i64> = rhs.value.values.iter()
                .map(|value| operation(value, lhs_value))
                .collect();
            Ok(self.spanned(Value::new(rhs.value.shape, transformed_values)))
        } else if rhs.value.shape.is_scalar() {
            let rhs_value = rhs.value.values.first().unwrap();
            let transformed_values: Vec<i64> = lhs.value.values.iter()
                .map(|value| operation(value, rhs_value))
                .collect();
            Ok(self.spanned(Value::new(lhs.value.shape, transformed_values)))
        } else if lhs.value.shape == rhs.value.shape {
            let transformed_values: Vec<i64> = lhs.value.values.iter().zip(rhs.value.values)
                .map(|(lhs_value, rhs_value)| operation(lhs_value, &rhs_value))
                .collect();
            Ok(self.spanned(Value::new(lhs.value.shape, transformed_values)))
        } else {
            Err(self.spanned(RuntimeError::InvalidArgumentType))
        }
    }

    fn unary_operation(&mut self, operation: impl Fn(&i64) -> i64) -> EvaluateResult {
        if let Some(operand) = self.stack.pop() {
            let transformed_values: Vec<i64> = operand.value.values.iter()
                .map(operation)
                .collect();
            Ok(self.spanned(Value::new(operand.value.shape, transformed_values)))
        } else {
            Err(self.spanned(RuntimeError::InvalidNumberOfArguments(1, self.stack.len() as u8)))
        }
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult {
    let nodes = parse(source);
    let mut interpreter = Interpreter::new(nodes);
    interpreter.interpret()
}
