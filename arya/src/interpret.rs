use crate::lex::Span;
// use crate::parse::{parse, Node, NodeValue, Op, ParseErrorKind};
//
// #[derive(Debug)]
// pub enum RuntimeErrorKind {
//     Parse(ParseErrorKind),
//     MissingArguments(i32),
//     InvalidArgumentType,
//     DifferentArrayElementTypes,
//     DifferentArrayElementShapes,
// }
//
// #[derive(Debug)]
// pub struct RuntimeError {
//     pub kind: RuntimeErrorKind,
//     pub span: Span
// }
//
// impl RuntimeError {
//     pub fn new(kind: RuntimeErrorKind, span: Span) -> Self {
//         Self { kind, span }
//     }
// }
//
// #[derive(Clone, Debug)]
// pub enum RuntimeValue {
//     Integer(i64),
//     Array(Box<Arr>)
// }
//
// impl RuntimeValue {
//     pub fn shape(&self) -> Shape {
//         match self {
//             RuntimeValue::Integer(_) |
//             RuntimeValue::Array(arr) => arr.shape.clone()
//         }
//     }
// }
//
// #[derive(Clone, Debug, PartialEq, Eq)]
// pub struct Shape {
//     dimensions: Vec<usize>
// }
//
// impl Shape {
//     pub const ZERO_DIMENSIONS: Shape = Self { dimensions: vec![] };
//
//     pub fn add_dimension(&mut self, size: usize) {
//         self.dimensions.push(size);
//     }
//
//     pub fn prepend_dimension(&mut self, size: usize) {
//         self.dimensions.insert(size, 0)
//     }
// }
//
// #[derive(Clone, Debug)]
// pub struct Arr {
//     pub shape: Shape,
//     pub elements: Vec<RuntimeValue>
// }
//
// impl Arr {
//     pub const EMPTY: Arr = Self { shape: Shape::ZERO_DIMENSIONS, elements: Vec::new() };
//
//     pub fn new(shape: Shape, elements: Vec<RuntimeValue>) -> Self {
//         Self { shape, elements }
//     }
// }
//
// pub type InterpretResult = Result<Vec<RuntimeValue>, RuntimeError>;
//
// pub struct Interpreter {
//     nodes: Vec<Node>,
//     stack: Vec<RuntimeValue>
// }
//
// impl Interpreter {
//     pub fn new(nodes: Vec<Node>) -> Self {
//         Self { nodes, stack: Vec::new() }
//     }
//
//     pub fn interpret(&mut self) -> InterpretResult {
//         while let Some(node) = self.nodes.pop() {
//             match self.evaluate(&node) {
//                 Ok(value) => self.stack.push(value),
//                 Err(error) => return Err(error)
//             }
//         }
//
//         Ok(self.stack.clone())
//     }
//
//     fn evaluate(&mut self, node: &Node) -> Result<RuntimeValue, RuntimeError> {
//         match &node.value {
//             NodeValue::Integer(i) => Ok(RuntimeValue::Integer(i.clone())),
//             NodeValue::Array(arr) => self.array(&node, &arr),
//             NodeValue::Operator(op) =>
//                 match op {
//                     Op::Plus => {
//                         let right = self.stack.pop().ok_or_else(|| RuntimeError::new(RuntimeErrorKind::MissingArguments(2), node.span.clone()))?;
//                         let left = self.stack.pop().ok_or_else(|| RuntimeError::new(RuntimeErrorKind::MissingArguments(1), node.span.clone()))?;
//                         match (left, right) {
//                             (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => Ok(RuntimeValue::Integer(l + r)),
//                             (RuntimeValue::Integer(l), RuntimeValue::Array(arr)) => {
//                                 if arr.elements.len() == 0 {
//                                     Ok(RuntimeValue::Array(Box::new(Arr::new(Shape::ZERO_DIMENSIONS, Vec::new()))))
//                                 } else {
//                                     arr.elements.iter().map(|e| {
//                                         match e {
//                                             RuntimeValue::Integer(r) => Ok(RuntimeValue::Integer(l + r)),
//                                             _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidArgumentType, node.span.clone()))
//                                         }
//                                     }).collect::<Result<Vec<RuntimeValue>, RuntimeError>>().map(|values| {
//                                         RuntimeValue::Array(Box::new(Arr::new(arr.shape.clone(), values)))
//                                     })
//                                 }
//                             }
//                             _ => Err(RuntimeError::new(RuntimeErrorKind::InvalidArgumentType, node.span.clone()))
//                         }
//                     }
//                     Op::Minus => {
//                         todo!()
//                     }
//                     Op::Multiply => {
//                         todo!()
//                     }
//                     Op::Divide => {
//                         todo!()
//                     }
//                 }
//         }
//     }
//
//     fn array(&mut self, node: &Node, nodes: &Vec<Node>) -> Result<RuntimeValue, RuntimeError> {
//         if nodes.len() == 0 {
//             return Ok(RuntimeValue::Array(Box::new(Arr::EMPTY)));
//         }
//
//         // Get the first element's shape and discriminant to ensure the array is homogenous
//         let head = self.evaluate(&nodes[0])?;
//         let head_shape = head.shape();
//         let mut array_shape = head.shape();
//         let array_discriminant = std::mem::discriminant(&head);
//
//         let mut elements = Vec::with_capacity(nodes.len());
//         match head {
//             RuntimeValue::Integer(_) |
//             RuntimeValue::Character(_) |
//             RuntimeValue::String(_) => {
//                 elements.push(head);
//             },
//             RuntimeValue::Array(arr) => {
//                 array_shape.add_dimension(arr.elements.len());
//                 for element in arr.elements {
//                     elements.push(element);
//                 }
//             }
//         }
//
//         for node in &nodes[1..] {
//             let element = self.evaluate(node)?;
//
//             let shape = element.shape();
//             if shape != head_shape {
//                 return Err(RuntimeError::new(RuntimeErrorKind::DifferentArrayElementShapes, node.span.clone()));
//             }
//
//             let discriminant = std::mem::discriminant(&element);
//             if discriminant != array_discriminant {
//                 return Err(RuntimeError::new(RuntimeErrorKind::DifferentArrayElementTypes, node.span.clone()));
//             }
//
//             elements.push(element)
//         }
//
//         array_shape.prepend_dimension(nodes.len());
//         Ok(RuntimeValue::Array(Box::new(Arr::new(array_shape, elements))))
//     }
// }
//
// pub fn interpret<'a>(source: &str) -> InterpretResult {
//     match parse(source) {
//         Ok(nodes) => Interpreter::new(nodes).interpret(),
//         Err(error) => Err(RuntimeError::new(RuntimeErrorKind::Parse(error.kind), error.span))
//     }
// }
