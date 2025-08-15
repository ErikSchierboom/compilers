use crate::lex::Span;
use crate::parse::{parse, Node, NodeValue, Op, ParseErrorKind};
use ecow::EcoString;

#[derive(Debug)]
pub enum RuntimeErrorKind {
    Parse(ParseErrorKind),
    MissingArguments(i32),
    InvalidArgumentType,
}

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Span
}

impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Clone, Debug)]
pub enum RuntimeValue {
    Integer(i64),
    Character(char),
    String(EcoString),
    Array(Vec<Self>)
}


pub type InterpretResult = Result<Vec<RuntimeValue>, RuntimeError>;

pub struct Interpreter {
    nodes: Vec<Node>,
    stack: Vec<RuntimeValue>
}

impl Interpreter {
    pub fn new(nodes: Vec<Node>) -> Self {
        Self { nodes, stack: Vec::new() }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        while let Some(node) = self.nodes.pop() {
            self.evaluate(node);
        }

        Ok(self.stack.clone())
    }

    fn evaluate(&mut self, node: Node) -> Result<RuntimeValue, RuntimeError> {
        match node.value {
            NodeValue::Integer(i) => Ok(RuntimeValue::Integer(i)),
            NodeValue::Character(c) => Ok(RuntimeValue::Character(c)),
            NodeValue::String(str) => Ok(RuntimeValue::String(str)),
            NodeValue::Array(arr) =>
                arr.into_iter()
                    .map(|elem|self.evaluate(elem))
                    .collect::<Result<Vec<RuntimeValue>, RuntimeError>>()
                    .map(RuntimeValue::Array),
            NodeValue::Operator(op) =>
                match op {
                    Op::Plus => {
                        let right = self.stack.pop().ok_or_else(|| RuntimeError::new(RuntimeErrorKind::MissingArguments(2), node.span.clone()))?;
                        let left = self.stack.pop().ok_or_else(|| RuntimeError::new(RuntimeErrorKind::MissingArguments(1), node.span.clone()))?;
                        match (left, right) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => Ok(RuntimeValue::Integer(l + r)),
                            (RuntimeValue::Integer(l), RuntimeValue::Array(elements)) => {
                                todo!("Get lement")
                                // if elements.iter().all(|e|matches!(RuntimeValue::Integer(_))) {
                                //     Ok(RuntimeValue::Array(l + r)) },
                                // }
                            },
                            _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidArgumentType, node.span.clone()))
                        }
                    }
                    Op::Minus => {
                        todo!()
                    }
                    Op::Multiply => {
                        todo!()
                    }
                    Op::Divide => {
                        todo!()
                    }
                }
        }
    }
}

pub fn interpret<'a>(source: &str) -> InterpretResult {
    match parse(source) {
        Ok(nodes) => Interpreter::new(nodes).interpret(),
        Err(error) => Err(RuntimeError::new(RuntimeErrorKind::Parse(error.kind), error.span))
    }
}
