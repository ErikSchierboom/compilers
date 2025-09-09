// use crate::location::{Span, Spanned};
// use crate::parser::{
//     parse, AnonymousFunction, Function, ParseError, ParseWordResult, PrimitiveFunction, Word,
// };
// use std::collections::VecDeque;
// use std::error::Error;
// use std::fmt::{Display, Formatter};
// use std::iter::Peekable;
// 
// #[derive(Debug)]
// pub enum RuntimeError {
//     Parse(ParseError),
//     InvalidNumberOfArguments(u8, u8),
//     IncompatibleShapes,
//     DifferentArrayElementShapes,
//     UnknownSymbol(String),
//     SymbolAlreadyExists(String),
//     InvalidArgumentType(String, String),
// }
// 
// impl Display for RuntimeError {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
//             RuntimeError::InvalidNumberOfArguments(expected, actual) => {
//                 write!(f, "Expected {expected} arguments, got {actual}")
//             }
//             RuntimeError::IncompatibleShapes => write!(f, "Incompatible shapes"),
//             RuntimeError::DifferentArrayElementShapes => {
//                 write!(f, "Not all rows in the array have the same shape")
//             }
//             RuntimeError::UnknownSymbol(name) => write!(f, "Unknown identifier: {name}"),
//             RuntimeError::SymbolAlreadyExists(name) => {
//                 write!(f, "Identifier already exists: {name}")
//             }
//             RuntimeError::InvalidArgumentType(expected, actual) => write!(f, "Invalid argument. Expected: {expected}, actual: {actual}")
//         }
//     }
// }
// 
// impl Error for RuntimeError {}
// 

// 
// #[derive(Clone, Debug)]
// pub enum Value {
//     Array(Array),
//     Function(Function),
// }
// 
// impl Value {
//     fn as_array(&self) -> Option<&Array> {
//         match self {
//             Value::Array(array) => Some(array),
//             _ => None
//         }
//     }
// 
//     fn as_function(&self) -> Option<&Function> {
//         match self {
//             Value::Function(function) => Some(function),
//             _ => None
//         }
//     }
// }
// 
// impl Display for Value {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Value::Array(array) => write!(f, "{array}"),
//             Value::Function(function) => write!(f, "{:?}", function),
//         }
//     }
// }
// 
// #[derive(Clone, Debug)]
// pub struct Array {
//     pub shape: Shape,
//     pub values: Vec<i64>,
// }
// 
// impl Array {
//     pub fn new(shape: Shape, elements: Vec<i64>) -> Self {
//         Self {
//             shape,
//             values: elements,
//         }
//     }
// 
//     pub fn scalar(element: i64) -> Self {
//         Self::new(Shape::SCALAR, vec![element])
//     }
// 
//     pub fn rows(&self) -> impl Iterator<Item=&[i64]> {
//         let row_len = self.shape.dimensions.iter().skip(1).product();
//         self.values.chunks(row_len)
//     }
// }
// 

// 
// pub type EvaluateResult = Result<(), Spanned<RuntimeError>>;
// pub type InterpretResult = Result<Vec<Value>, Spanned<RuntimeError>>;
// 
// pub struct Interpreter<T>
// where
//     T: Iterator<Item=ParseWordResult>,
// {
//     words: Peekable<T>,
//     queue: VecDeque<Spanned<Word>>,
//     stack: Vec<Value>,
//     span: Span,
// }
// 
// impl<T> Interpreter<T>
// where
//     T: Iterator<Item=ParseWordResult>,
// {
//     pub fn new(nodes: T) -> Self {
//         Self {
//             words: nodes.peekable(),
//             queue: VecDeque::new(),
//             stack: Vec::new(),
//             span: Span::EMPTY,
//         }
//     }
// 
//     pub fn interpret(&mut self) -> InterpretResult {
//         while let Some(node) = self.next() {
//             match node {
//                 Ok(node) => self.evaluate(node)?,
//                 Err(error) => {
//                     self.span = error.span.clone();
//                     return Err(self.spanned(RuntimeError::Parse(error.value)));
//                 }
//             }
//         }
// 
//         Ok(self.stack.clone())
//     }
// 
//     fn next(&mut self) -> Option<ParseWordResult> {
//         match self.queue.pop_front() {
//             None => self.words.next(),
//             Some(word) => Some(Ok(word))
//         }
//     }
// 
//     fn evaluate(&mut self, word: Spanned<Word>) -> EvaluateResult {
//         self.span = word.span.clone();
// 
//         match word.value {
//             Word::Integer(i) => self.integer(i),
//             Word::Function(func) => self.function(*func),
//             Word::Array(elements) => self.array(elements),
//         }
//     }
// 
//     fn evaluate_words(&mut self, words: Vec<Spanned<Word>>) -> EvaluateResult {
//         for word in words {
//             self.evaluate(word)?
//         }
//         Ok(())
//     }
// 
//     fn integer(&mut self, i: i64) -> EvaluateResult {
//         let value = Value::Array(Array::scalar(i.clone()));
//         self.push(value);
//         Ok(())
//     }
// 
//     fn function(&mut self, func: Function) -> EvaluateResult {
//         match func {
//             Function::Anonymous(anonymous_func) => self.anonymous_function(anonymous_func),
//             Function::Primitive(primitive_func) => self.primitive_function(primitive_func),
//         }
//     }
// 
//     fn anonymous_function(&mut self, func: AnonymousFunction) -> EvaluateResult {
//         self.push(Value::Function(Function::Anonymous(func)));
//         Ok(())
//     }
// 
//     fn primitive_function(&mut self, func: PrimitiveFunction) -> EvaluateResult {
//         match func {
//             PrimitiveFunction::Add => self.binary_operation(|l, r| l + r),
//             PrimitiveFunction::Subtract => self.binary_operation(|l, r| l - r),
//             PrimitiveFunction::Multiply => self.binary_operation(|l, r| l * r),
//             PrimitiveFunction::Divide => self.binary_operation(|l, r| l / r),
//             PrimitiveFunction::And => self.binary_operation(|l, r| l & r),
//             PrimitiveFunction::Or => self.binary_operation(|l, r| l | r),
//             PrimitiveFunction::Xor => self.binary_operation(|l, r| l ^ r),
//             PrimitiveFunction::Equal => self.binary_operation(|l, r| (l == r) as i64),
//             PrimitiveFunction::NotEqual => self.binary_operation(|l, r| (l != r) as i64),
//             PrimitiveFunction::Greater => self.binary_operation(|l, r| (l > r) as i64),
//             PrimitiveFunction::GreaterEqual => self.binary_operation(|l, r| (l >= r) as i64),
//             PrimitiveFunction::Less => self.binary_operation(|l, r| (l < r) as i64),
//             PrimitiveFunction::LessEqual => self.binary_operation(|l, r| (l <= r) as i64),
//             PrimitiveFunction::Not => self.unary_operation(|value| !value),
//             PrimitiveFunction::Negate => self.unary_operation(|value| -value),
//             PrimitiveFunction::Dup => {
//                 self.unary_stack_operation(|value| vec![value.clone(), value.clone()])
//             }
//             PrimitiveFunction::Drop => self.unary_stack_operation(|_| vec![]),
//             PrimitiveFunction::Swap => {
//                 self.binary_stack_operation(|lhs, rhs| vec![rhs.clone(), lhs.clone()])
//             }
//             PrimitiveFunction::Over => {
//                 self.binary_stack_operation(|lhs, rhs| vec![lhs.clone(), rhs.clone(), lhs.clone()])
//             }
//             PrimitiveFunction::Reduce => {
//                 todo!()
//                 // self.binary_stack_operation(|lhs, rhs| {
//                 //     // TODO: error handling of stack values
//                 //     let func = rhs.as_function().unwrap().to_owned();
//                 //     if func.signature().num_inputs != 2 {}
//                 //
//                 //     let array = lhs.as_array().unwrap();
//                 //
//                 //     let reduced = array.rows().reduce(|acc, e| {
//                 //         self.stack.push(Value::Array(Array::new(Shape::SCALAR, acc.clone().to_vec())));
//                 //         self.stack.push(Value::Array(Array::new(Shape::SCALAR, e.clone().to_vec())));
//                 //         self.function(func.clone());
//                 //         let arr = self.pop();
//                 //         let a = arr.unwrap().as_array().unwrap().clone();
//                 //         a.values.as_slice()
//                 //     });
//                 //
//                 //     // TODO: error handling?
//                 //     vec![Value::Array(Array::new(array.shape.clone(), reduced.unwrap().to_vec()))]
//                 // });
//                 //
//                 // Ok(())
//             }
//         }
//     }
// 
//     fn array(&mut self, elements: Vec<Spanned<Word>>) -> EvaluateResult {
//         let mut array_shape: Option<Shape> = None;
//         let mut array_values: Vec<i64> = Vec::new();
//         let num_rows = elements.len();
// 
//         for spanned_element in elements {
//             self.evaluate(spanned_element)?;
//             let value = self.pop().unwrap();
//             let array = value
//                 .as_array()
//                 .ok_or_else(|| self.spanned(RuntimeError::InvalidArgumentType("Function".to_string(), "Array".to_string())))?; // TODO: error handling
// 
//             let existing_shape = array_shape.get_or_insert(array.shape.clone());
//             if *existing_shape != array.shape {
//                 return self.error(RuntimeError::DifferentArrayElementShapes);
//             }
// 
//             for integer in &array.values {
//                 array_values.push(integer.clone())
//             }
//         }
// 
//         let mut shape = array_shape.get_or_insert(Shape::SCALAR).clone();
//         shape.prepend_dimension(num_rows);
// 
//         let value = Value::Array(Array::new(shape, array_values));
//         self.push(value);
// 
//         Ok(())
//     }
// 
//     fn push(&mut self, value: Value) {
//         self.stack.push(value)
//     }
// 
//     fn pop(&mut self) -> Option<Value> {
//         self.stack.pop()
//     }
// 
//     fn spanned<V>(&self, value: V) -> Spanned<V> {
//         Spanned::new(value, self.span.clone())
//     }
// 
//     fn binary_operation(&mut self, operation: impl Fn(&i64, &i64) -> i64) -> EvaluateResult {
//         self.verify_stack_size(2)?;
// 
//         let rhs = self.pop().unwrap();
//         let lhs = self.pop().unwrap();
// 
//         match (lhs, rhs) {
//             (Value::Array(lhs_value), Value::Array(rhs_value)) => {
//                 if lhs_value.shape.is_scalar() {
//                     let lhs_value = lhs_value.values.first().unwrap();
//                     let transformed_values: Vec<i64> = rhs_value
//                         .values
//                         .iter()
//                         .map(|value| operation(value, lhs_value))
//                         .collect();
//                     let value = Value::Array(Array::new(rhs_value.shape, transformed_values));
//                     self.push(value);
//                     Ok(())
//                 } else if rhs_value.shape.is_scalar() {
//                     let rhs_value = rhs_value.values.first().unwrap();
//                     let transformed_values: Vec<i64> = lhs_value
//                         .values
//                         .iter()
//                         .map(|value| operation(value, rhs_value))
//                         .collect();
//                     let value = Value::Array(Array::new(lhs_value.shape, transformed_values));
//                     self.push(value);
//                     Ok(())
//                 } else if lhs_value.shape == rhs_value.shape {
//                     let transformed_values: Vec<i64> = lhs_value
//                         .values
//                         .iter()
//                         .zip(rhs_value.values)
//                         .map(|(lhs_value, rhs_value)| operation(lhs_value, &rhs_value))
//                         .collect();
//                     let value = Value::Array(Array::new(lhs_value.shape, transformed_values));
//                     self.push(value);
//                     Ok(())
//                 } else {
//                     self.error(RuntimeError::IncompatibleShapes)
//                 }
//             }
//             _ => self.error(RuntimeError::InvalidArgumentType("Array".to_string(), "Function".to_string())),
//         }
//     }
// 
//     fn unary_operation(&mut self, operation: impl Fn(&i64) -> i64) -> EvaluateResult {
//         self.verify_stack_size(1)?;
// 
//         let operand = self.pop().unwrap();
//         match operand {
//             Value::Array(array) => {
//                 let transformed_values: Vec<i64> = array.values.iter().map(operation).collect();
//                 let value = Value::Array(Array::new(array.shape, transformed_values));
//                 self.push(value);
//             }
//             _ => return self.error(RuntimeError::InvalidArgumentType("Array".to_string(), "Function".to_string()))
//         }
// 
//         Ok(())
//     }
// 
//     fn binary_stack_operation(
//         &mut self,
//         operation: impl Fn(&Value, &Value) -> Vec<Value>,
//     ) -> EvaluateResult {
//         self.verify_stack_size(2)?;
// 
//         let rhs = self.pop().unwrap();
//         let lhs = self.pop().unwrap();
//         for value in operation(&lhs, &rhs) {
//             self.push(value)
//         }
// 
//         Ok(())
//     }
// 
//     fn unary_stack_operation(
//         &mut self,
//         operation: impl Fn(&Value) -> Vec<Value>,
//     ) -> EvaluateResult {
//         self.verify_stack_size(1)?;
// 
//         let operand = self.pop().unwrap();
//         for value in operation(&operand) {
//             self.push(value)
//         }
// 
//         Ok(())
//     }
// 
//     fn verify_stack_size(&mut self, expected: u8) -> Result<(), Spanned<RuntimeError>> {
//         let actual = self.stack.len() as u8;
//         if actual < expected {
//             Err(self.spanned(RuntimeError::InvalidNumberOfArguments(
//                 expected,
//                 self.stack.len() as u8,
//             )))
//         } else {
//             Ok(())
//         }
//     }
// 
//     fn error(&mut self, error: RuntimeError) -> EvaluateResult {
//         Err(self.spanned(error))
//     }
// }
// 
// pub fn interpret<'a>(source: &str) -> InterpretResult {
//     let words = parse(source);
//     let mut interpreter = Interpreter::new(words);
//     interpreter.interpret()
// }
