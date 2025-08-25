use std::collections::HashMap;
use crate::location::{Span, Spanned};
use crate::parser::{parse, Node, Op, ParseError, ParseNodeResult};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError),
    InvalidNumberOfArguments(u8, u8),
    IncompatibleShapes,
    DifferentArrayElementShapes,
    UnknownIdentifier(String),
    IdentifierAlreadyExists(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
            RuntimeError::InvalidNumberOfArguments(expected, actual) => write!(f, "Expected {expected} arguments, got {actual}"),
            RuntimeError::IncompatibleShapes => write!(f, "Incompatible shapes"),
            RuntimeError::DifferentArrayElementShapes => write!(f, "Not all rows in the array have the same shape"),
            RuntimeError::UnknownIdentifier(name) => write!(f, "Unknown identifier: {name}"),
            RuntimeError::IdentifierAlreadyExists(name) => write!(f, "Identifier already exists: {name}"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Shape {
    dimensions: Vec<usize>,
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
pub enum Value {
    Array(Array)
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Array(array) => write!(f, "{array}")
        }
    }
}

#[derive(Clone, Debug)]
pub struct Array {
    pub shape: Shape,
    pub values: Vec<i64>,
}

impl Array {
    pub fn new(shape: Shape, elements: Vec<i64>) -> Self {
        Self {
            shape,
            values: elements,
        }
    }

    pub fn scalar(element: i64) -> Self {
        Self::new(Shape::SCALAR, vec![element])
    }
}

impl Display for Array {
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
            }
            2 => {
                let num_columns = self.shape.dimensions.get(1).unwrap();
                let max_width = self
                    .values
                    .iter()
                    .map(|&value| value.checked_ilog10().unwrap_or(0) + 1)
                    .max()
                    .unwrap_or(1) as usize;

                write!(f, "[")?;
                for (i, v) in self.values.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?
                    }
                    write!(f, "{:>max_width$}", v)?;
                    if i % num_columns == num_columns - 1 && i < self.values.len() - 1 {
                        write!(f, "\n")?;
                    }
                }
                write!(f, "]")
            }
            _ => write!(f, "{:?}", self),
        }
    }
}

pub type EvaluateResult = Result<(), Spanned<RuntimeError>>;
pub type InterpretResult = Result<Vec<Spanned<Value>>, Spanned<RuntimeError>>;

pub struct Interpreter<T>
where
    T: Iterator<Item = ParseNodeResult>,
{
    nodes: Peekable<T>,
    bindings: HashMap<String, Vec<Spanned<Node>>>,
    stack: Vec<Spanned<Value>>,
    span: Span,
}

impl<T> Interpreter<T>
where
    T: Iterator<Item = ParseNodeResult>,
{
    pub fn new(nodes: T) -> Self {
        Self {
            nodes: nodes.peekable(),
            bindings: HashMap::new(),
            stack: Vec::new(),
            span: Span::EMPTY,
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        while let Some(node) = self.nodes.next() {
            match node {
                Ok(node) => {
                    self.span = node.span.clone();
                    self.evaluate(&node)?
                }
                Err(error) => {
                    self.span = error.span.clone();
                    return Err(self.spanned(RuntimeError::Parse(error.value)));
                }
            }
        }

        Ok(self.stack.clone())
    }

    fn evaluate(&mut self, node: &Spanned<Node>) -> EvaluateResult {
        match &node.value {
            Node::Integer(i) => self.integer(i),
            Node::Operation(op) => self.operator(op),
            Node::Array(elements) => self.array(elements),
            Node::Identifier(name) => {
                let binding = &self.bindings.get_mut(name);
                match binding {
                    None => self.error(RuntimeError::UnknownIdentifier(name.clone())),
                    Some(nodes) => {
                        for node in nodes.to_vec() {
                            self.evaluate(&node)?
                        }
                        Ok(())
                    }
                }
            },
            Node::Binding(name, body) => match self.bindings.insert(name.clone(), body.to_vec()) {
                None => Ok(()),
                Some(_) => self.error(RuntimeError::IdentifierAlreadyExists(name.clone()))
            }
        }
    }

    fn integer(&mut self, i: &i64) -> EvaluateResult {
        let value = Value::Array(Array::scalar(i.clone()));
        self.push(value);
        Ok(())
    }

    fn operator(&mut self, op: &Op) -> EvaluateResult {
        match op {
            Op::Add => self.binary_operation(|l, r| l + r),
            Op::Subtract => self.binary_operation(|l, r| l - r),
            Op::Multiply => self.binary_operation(|l, r| l * r),
            Op::Divide => self.binary_operation(|l, r| l / r),
            Op::And => self.binary_operation(|l, r| l & r),
            Op::Or => self.binary_operation(|l, r| l | r),
            Op::Xor => self.binary_operation(|l, r| l ^ r),
            Op::Equal => self.binary_operation(|l, r| (l == r) as i64),
            Op::NotEqual => self.binary_operation(|l, r| (l != r) as i64),
            Op::Greater => self.binary_operation(|l, r| (l > r) as i64),
            Op::GreaterEqual => self.binary_operation(|l, r| (l >= r) as i64),
            Op::Less => self.binary_operation(|l, r| (l < r) as i64),
            Op::LessEqual => self.binary_operation(|l, r| (l <= r) as i64),
            Op::Not => self.unary_operation(|value| !value),
            Op::Negate => self.unary_operation(|value| -value),
            Op::Dup => self.unary_stack_operation(|value| vec![value.clone(), value.clone()]),
            Op::Drop => self.unary_stack_operation(|_| vec![]),
            Op::Swap => self.binary_stack_operation(|lhs, rhs| vec!(rhs.clone(), lhs.clone())),
            Op::Over => self.binary_stack_operation(|lhs, rhs| vec!(lhs.clone(), rhs.clone(), lhs.clone())),
        }
    }

    fn array(&mut self, elements: &Vec<Spanned<Node>>) -> EvaluateResult {
        let mut array_shape: Option<Shape> = None;
        let mut array_values: Vec<i64> = Vec::new();

        for spanned_element in elements {
            self.evaluate(spanned_element)?;
            let spanned_value = self.pop().unwrap();
            match spanned_value.value {
                Value::Array(array) => {
                    let value_shape = array.shape;
                    let existing_shape = array_shape.get_or_insert(value_shape.clone());
                    if *existing_shape != value_shape {
                        self.span = spanned_element.span.clone();
                        return self.error(RuntimeError::DifferentArrayElementShapes);
                    }

                    for integer in array.values {
                        array_values.push(integer)
                    }
                }
            }
        }

        let mut shape = array_shape.get_or_insert(Shape::SCALAR).clone();
        shape.prepend_dimension(elements.len());

        let value = Value::Array(Array::new(shape, array_values));
        self.push(value);

        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(self.spanned(value))
    }

    fn pop(&mut self) -> Option<Spanned<Value>> {
        self.stack.pop()
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }

    fn binary_operation(&mut self, operation: impl Fn(&i64, &i64) -> i64) -> EvaluateResult {
        self.verify_stack_size(2)?;

        let rhs = self.pop().unwrap();
        let lhs = self.pop().unwrap();

        match (lhs.value, rhs.value) {
            (Value::Array(lhs_value), Value::Array(rhs_value)) => {
                if lhs_value.shape.is_scalar() {
                    let lhs_value = lhs_value.values.first().unwrap();
                    let transformed_values: Vec<i64> = rhs_value
                        .values
                        .iter()
                        .map(|value| operation(value, lhs_value))
                        .collect();
                    let value = Value::Array(Array::new(rhs_value.shape, transformed_values));
                    self.push(value);
                    Ok(())
                } else if rhs_value.shape.is_scalar() {
                    let rhs_value = rhs_value.values.first().unwrap();
                    let transformed_values: Vec<i64> = lhs_value
                        .values
                        .iter()
                        .map(|value| operation(value, rhs_value))
                        .collect();
                    let value = Value::Array(Array::new(lhs_value.shape, transformed_values));
                    self.push(value);
                    Ok(())
                } else if lhs_value.shape == rhs_value.shape {
                    let transformed_values: Vec<i64> = lhs_value
                        .values
                        .iter()
                        .zip(rhs_value.values)
                        .map(|(lhs_value, rhs_value)| operation(lhs_value, &rhs_value))
                        .collect();
                    let value = Value::Array(Array::new(lhs_value.shape, transformed_values));
                    self.push(value);
                    Ok(())
                } else {
                    self.error(RuntimeError::IncompatibleShapes)
                }
            }
        }
    }

    fn unary_operation(&mut self, operation: impl Fn(&i64) -> i64) -> EvaluateResult {
        self.verify_stack_size(1)?;

        let operand = self.pop().unwrap();
        match operand.value {
            Value::Array(array) => {
                let transformed_values: Vec<i64> = array.values.iter().map(operation).collect();
                let value = Value::Array(Array::new(array.shape, transformed_values));
                self.push(value);
            }
        }

        Ok(())
    }

    fn binary_stack_operation(
        &mut self,
        operation: impl Fn(&Value, &Value) -> Vec<Value>,
    ) -> EvaluateResult {
        self.verify_stack_size(2)?;

        let rhs = self.pop().unwrap();
        let lhs = self.pop().unwrap();
        for value in operation(&lhs.value, &rhs.value) {
            self.push(value)
        }

        Ok(())
    }

    fn unary_stack_operation(
        &mut self,
        operation: impl Fn(&Value) -> Vec<Value>,
    ) -> EvaluateResult {
        self.verify_stack_size(1)?;

        let operand = self.pop().unwrap();
        for value in operation(&operand.value) {
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

    fn error(&mut self, error: RuntimeError) -> EvaluateResult {
        Err(self.spanned(error))
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult {
    let nodes = parse(source);
    let mut interpreter = Interpreter::new(nodes);
    interpreter.interpret()
}
