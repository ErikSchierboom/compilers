// use crate::array::{DimensionalArray, Shape};
// use crate::environment::Environment;
// use crate::parser::{parse, DyadicOperation, MonadicOperation, NiladicOperation, ParseError, ParseResult, Word};
// use std::error::Error;
// use std::fmt::{Display, Formatter};
// use std::iter::Peekable;
// use std::ops::{Add, Div, Mul, Sub};
//
// #[derive(Debug, PartialEq)]
// pub enum RuntimeError {
//     Parse(ParseError),
//     EmptyStack,
//     UnsupportedArgumentTypes, // TODO: store allows argument types
//     IncompatibleArrayShapes,
//     IncompatibleArrayValues,
//     UnknownIdentifier(String),
// }
//
// impl Display for RuntimeError {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             RuntimeError::Parse(parse_error) => write!(f, "{parse_error}"),
//             RuntimeError::EmptyStack => write!(f, "Missing argument"),
//             RuntimeError::UnsupportedArgumentTypes => write!(f, "Unsupported argument types"),
//             RuntimeError::IncompatibleArrayShapes => write!(f, "Incompatible array shapes"),
//             RuntimeError::IncompatibleArrayValues => write!(f, "Incompatible array values"),
//             RuntimeError::UnknownIdentifier(identifier) => write!(f, "Unknown identifier '{identifier}'"),
//         }
//     }
// }
//
// impl Error for RuntimeError {}
//
// // TODO: add box type to allow for not special-casing regular values
// #[derive(Debug, Clone, PartialEq)]
// pub enum Value {
//     Char(char),
//     Number(f64),
//     String(String),
//     Lambda(Vec<Word>),
//     Array(Array),
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub enum Array {
//     Empty,
//     Char(DimensionalArray<char>),
//     Number(DimensionalArray<f64>),
//     String(DimensionalArray<String>),
// }
//
// impl Array {
//     pub fn shape(&self) -> Shape {
//         match self {
//             Array::Empty => Shape::empty(),
//             Array::Char(array) => array.shape.clone(),
//             Array::Number(array) => array.shape.clone(),
//             Array::String(array) => array.shape.clone()
//         }
//     }
// }
//
// impl Display for Array {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Array::Empty => write!(f, "[]"),
//             Array::Char(array) => write!(f, "{}", array),
//             Array::Number(array) => write!(f, "{}", array),
//             Array::String(array) => write!(f, "{}", array),
//         }
//     }
// }
//
// impl Value {
//     pub fn take_char(&mut self) -> Option<char> {
//         match self {
//             Value::Char(c) => Some(*c),
//             _ => None
//         }
//     }
//
//     pub fn take_number(&mut self) -> Option<f64> {
//         match self {
//             Value::Number(number) => Some(*number),
//             _ => None
//         }
//     }
//
//     pub fn take_string(&mut self) -> Option<String> {
//         match self {
//             Value::String(string) => Some(string.clone()),
//             _ => None
//         }
//     }
//
//     pub fn take_lambda(&mut self) -> Option<Vec<Word>> {
//         match self {
//             Value::Lambda(words) => Some(words.clone()),
//             _ => None
//         }
//     }
//
//     fn shape(&self) -> Shape {
//         match self {
//             Value::Array(array) => array.shape(),
//             _ => Shape::empty()
//         }
//     }
//
//     fn array_value_kind(&self) -> ArrayValueKind {
//         match self {
//             Value::Array(array) => {
//                 match array {
//                     Array::Empty => ArrayValueKind::Empty,
//                     Array::Char(_) => ArrayValueKind::Char,
//                     Array::Number(_) => ArrayValueKind::Number,
//                     Array::String(_) => ArrayValueKind::String
//                 }
//             }
//             Value::Char(_) => ArrayValueKind::Char,
//             Value::Number(_) => ArrayValueKind::Number,
//             Value::String(_) => ArrayValueKind::String,
//             Value::Lambda(_) => panic!("Arrays should not contain lambdas")
//         }
//     }
// }
//
// impl Display for Value {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Value::Char(c) => write!(f, "{c}"),
//             Value::Number(int) => write!(f, "{int}"),
//             Value::String(string) => write!(f, "{string}"),
//             Value::Lambda(words) => write!(f, "({})", words.iter().map(|w| format!("{w}")).collect::<Vec<_>>().join(" ")),
//             Value::Array(array) => write!(f, "{array}"),
//         }
//     }
// }
//
// #[derive(Debug, Clone, PartialEq)]
// pub enum ArrayValueKind {
//     Empty,
//     Char,
//     Number,
//     String,
//     Lambda,
// }
//
// #[derive(Clone)]
// pub enum Binding {
//     Custom(String),
//     Builtin(Builtin),
// }
//
// #[derive(Clone)]
// pub enum Builtin {
//     Max,
//     Min,
//     Abs,
//     Dup,
//     Swap,
//     Split,
// }
//
// impl Executable for Builtin {
//     fn execute(&self, env: &mut Environment) -> InterpretResult {
//         match self {
//             Builtin::Max => env.execute_dyadic_op(f64::max, Some(u8::max)),
//             Builtin::Min => env.execute_dyadic_op(f64::min, Some(u8::min)),
//             Builtin::Abs => env.execute_monadic_num_op(f64::abs),
//             Builtin::Dup => {
//                 let value = env.pop()?;
//                 env.push(value.clone());
//                 env.push(value);
//                 Ok(())
//             }
//             Builtin::Swap => {
//                 let right = env.pop()?;
//                 let left = env.pop()?;
//                 env.push(right);
//                 env.push(left);
//                 Ok(())
//             }
//             Builtin::Split => {
//                 let delimiters = env.pop()?;
//                 let string = env.pop()?;
//
//                 match (string, delimiters) {
//                     (Value::String(str), Value::Char(c)) => {
//                         todo!()
//                     }
//                     (Value::String(str), Value::String(c)) => {
//                         todo!()
//                     }
//                     (Value::String(str), Value::Array(Array::Char(chars))) => {
//                         todo!()
//                     }
//                     (Value::String(str), Value::Array(Array::String(strings))) => {
//                         todo!()
//                     }
//                     _ => Err(RuntimeError::UnsupportedArgumentTypes),
//                 }
//             }
//         }
//     }
// }
//
// pub trait Executable {
//     fn execute(&self, env: &mut Environment) -> InterpretResult;
// }
//
// impl Executable for Word {
//     fn execute(&self, env: &mut Environment) -> InterpretResult {
//         match self {
//             Word::Number(number) => env.push(Value::Number(number.clone())),
//             Word::String(string) => env.push(Value::String(string.clone())),
//             Word::Char(c) => env.push(Value::Char(c.clone())),
//             Word::Array(elements) => {
//                 let array = if elements.is_empty() {
//                     Array::Empty
//                 } else {
//                     for element in elements {
//                         element.value.execute(env)?;
//                     }
//
//                     let values = env.pop_n(elements.len())?;
//
//                     for window in values.windows(2) {
//                         if window[0].shape() != window[1].shape() {
//                             return Err(RuntimeError::IncompatibleArrayShapes);
//                         }
//
//                         if window[0].array_value_kind() != window[1].array_value_kind() {
//                             return Err(RuntimeError::IncompatibleArrayShapes);
//                         }
//                     }
//
//                     let first = values.first().unwrap();
//
//                     let mut shape = first.shape();
//                     shape.prepend_dimension(values.len());
//
//                     match first.array_value_kind() {
//                         ArrayValueKind::Empty => Array::Empty,
//                         ArrayValueKind::Char => Array::Char(DimensionalArray::new(shape, values.into_iter().map(|mut value| value.take_char().unwrap()).collect())),
//                         ArrayValueKind::Number => Array::Number(DimensionalArray::new(shape, values.into_iter().map(|mut value| value.take_number().unwrap()).collect())),
//                         ArrayValueKind::String => Array::String(DimensionalArray::new(shape, values.into_iter().map(|mut value| value.take_string().unwrap()).collect())),
//                         ArrayValueKind::Lambda => panic!("Arrays cannot contain lambdas")
//                     }
//                 };
//
//                 env.push(Value::Array(array))
//             }
//             Word::Lambda(words) => env.push(Value::Lambda(words.iter().map(|word| word.value.clone()).collect())),
//             Word::Identifier(identifier) => {
//                 match env.get(identifier)?.clone() {
//                     Binding::Builtin(builtin) => builtin.execute(env)?,
//                 }
//             }
//             Word::Niladic(niladic_op) => niladic_op.execute(env)?,
//             Word::Monadic(monadic_op) => monadic_op.execute(env)?,
//             Word::Dyadic(dyadic_op) => dyadic_op.execute(env)?,
//         }
//
//         Ok(())
//     }
// }
//
// impl Executable for NiladicOperation {
//     fn execute(&self, env: &mut Environment) -> InterpretResult {
//         match self {
//             NiladicOperation::Stack => {
//                 for value in env.stack.iter().rev() {
//                     println!("{value}")
//                 }
//                 Ok(())
//             }
//         }
//     }
// }
//
//
// impl Executable for MonadicOperation {
//     fn execute(&self, env: &mut Environment) -> InterpretResult {
//         match self {
//             MonadicOperation::Not => {
//                 let mut val = env.pop()?;
//
//                 match val {
//                     Value::Number(ref mut number) => {
//                         *number = 1. - *number;
//                         env.push(val);
//                         Ok(())
//                     }
//
//                     Value::Array(Array::Number(ref mut array)) => {
//                         for elem in &mut array.elements {
//                             *elem = 1. - *elem;
//                         }
//                         env.push(val);
//                         Ok(())
//                     }
//
//                     _ => Err(RuntimeError::UnsupportedArgumentTypes),
//                 }
//             }
//         }
//     }
// }
//
// impl Executable for DyadicOperation {
//     fn execute(&self, env: &mut Environment) -> InterpretResult {
//         match self {
//             DyadicOperation::Add => env.execute_dyadic_op(f64::add, Some(u8::add)),
//             DyadicOperation::Sub => env.execute_dyadic_op(f64::sub, Some(u8::sub)),
//             DyadicOperation::Mul => env.execute_dyadic_op(f64::mul, Some(u8::mul)),
//             DyadicOperation::Div => env.execute_dyadic_op(f64::div, Some(u8::div)),
//             DyadicOperation::Equal => todo!(),
//             DyadicOperation::NotEqual => todo!(),
//             DyadicOperation::Greater => todo!(),
//             DyadicOperation::GreaterEqual => todo!(),
//             DyadicOperation::Less => todo!(),
//             DyadicOperation::LessEqual => todo!(),
//         }
//     }
// }
//
// pub type InterpretResult<T = ()> = Result<T, RuntimeError>;
//
// pub struct Interpreter<T>
// where
//     T: Iterator<Item=ParseResult>,
// {
//     words: Peekable<T>,
//     environment: Environment,
// }
//
// impl<T> Interpreter<T>
// where
//     T: Iterator<Item=ParseResult>,
// {
//     pub fn new(words: T) -> Self {
//         Self { words: words.peekable(), environment: Environment::new() }
//     }
//
//     pub fn interpret(&mut self) -> InterpretResult<Vec<Value>> {
//         while let Some(parse_result) = self.next() {
//             match parse_result {
//                 Ok(word) => word.value.execute(&mut self.environment)?,
//                 Err(error) => return Err(RuntimeError::Parse(error.value))
//             }
//         }
//
//         Ok(self.environment.stack.clone())
//     }
//
//     fn next(&mut self) -> Option<ParseResult> {
//         self.words.next()
//     }
// }
//
// pub fn interpret<'a>(source: &str) -> InterpretResult<Vec<Value>> {
//     let words = parse(source);
//     let mut interpreter = Interpreter::new(words);
//     interpreter.interpret()
// }
//
// impl From<f64> for Value {
//     fn from(value: f64) -> Self {
//         Value::Number(value)
//     }
// }
//
// impl From<i64> for Value {
//     fn from(value: i64) -> Self {
//         Value::Number(value as f64)
//     }
// }
//
// impl From<char> for Value {
//     fn from(value: char) -> Self {
//         Value::Char(value)
//     }
// }
//
// impl<'a> From<&'a str> for Value {
//     fn from(value: &'a str) -> Self {
//         Value::String(value.to_string())
//     }
// }
//
// impl From<String> for Value {
//     fn from(value: String) -> Self {
//         Value::String(value)
//     }
// }
//
// // TODO: move this
// impl From<Vec<f64>> for Value {
//     fn from(values: Vec<f64>) -> Self {
//         Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![values.len()]), values)))
//     }
// }
//
// impl From<Vec<i64>> for Value {
//     fn from(values: Vec<i64>) -> Self {
//         let numbers: Vec<f64> = values.into_iter().map(|value| value as f64).collect();
//         Value::Array(Array::Number(DimensionalArray::new(Shape::new(vec![numbers.len()]), numbers)))
//     }
// }
//
// impl From<Vec<char>> for Value {
//     fn from(values: Vec<char>) -> Self {
//         Value::Array(Array::Char(DimensionalArray::new(Shape::new(vec![values.len()]), values.into())))
//     }
// }
//
// impl<'a> From<Vec<&'a str>> for Value {
//     fn from(values: Vec<&'a str>) -> Self {
//         let strings: Vec<String> = values.into_iter().map(|str| str.to_string()).collect();
//         Value::Array(Array::String(DimensionalArray::new(Shape::new(vec![strings.len()]), strings)))
//     }
// }
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn test_interpret_scalars() {
//         let tokens = interpret("19");
//         assert_eq!(Ok(vec![19.into()]), tokens);
//
//         let tokens = interpret("5.32");
//         assert_eq!(Ok(vec![5.32.into()]), tokens);
//
//         let tokens = interpret("'a'");
//         assert_eq!(Ok(vec!['a'.into()]), tokens);
//
//         let tokens = interpret(r#""hi there""#);
//         assert_eq!(Ok(vec!["hi there".into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_arrays() {
//         let tokens = interpret("[]");
//         assert_eq!(Ok(vec![Vec::<f64>::new().into()]), tokens);
//
//         let tokens = interpret("[1 2 3]");
//         assert_eq!(Ok(vec![vec![1, 2, 3].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_not() {
//         let tokens = interpret("1 !");
//         assert_eq!(Ok(vec![0.into()]), tokens);
//
//         let tokens = interpret("[0 1 2 3] !");
//         assert_eq!(Ok(vec![vec![1, 0, -1, -2].into()]), tokens);
//
//         let tokens = interpret("1.77 !");
//         assert_eq!(Ok(vec![(-0.77).into()]), tokens);
//
//         let tokens = interpret("[0.5 1.0 2.0 3.3] !");
//         assert_eq!(Ok(vec![vec![0.5, 0.0, -1.0, -2.3].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_abs() {
//         let tokens = interpret("1 abs");
//         assert_eq!(Ok(vec![1.into()]), tokens);
//
//         let tokens = interpret("-3.2 abs");
//         assert_eq!(Ok(vec![3.2.into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_max() {
//         let tokens = interpret("1 2 max");
//         assert_eq!(Ok(vec![2.into()]), tokens);
//
//         let tokens = interpret("3 2 max");
//         assert_eq!(Ok(vec![3.into()]), tokens);
//
//         let tokens = interpret("[0 1 4] 2 max");
//         assert_eq!(Ok(vec![vec![2, 2, 4].into()]), tokens);
//
//         let tokens = interpret("2 [0 1 4] max");
//         assert_eq!(Ok(vec![vec![2, 2, 4].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_min() {
//         let tokens = interpret("1 2 min");
//         assert_eq!(Ok(vec![1.into()]), tokens);
//
//         let tokens = interpret("3 2 min");
//         assert_eq!(Ok(vec![2.into()]), tokens);
//
//         let tokens = interpret("[0 1 4] 2 min");
//         assert_eq!(Ok(vec![vec![0, 1, 2].into()]), tokens);
//
//         let tokens = interpret("2 [0 1 4] min");
//         assert_eq!(Ok(vec![vec![0, 1, 2].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_addition_on_numbers() {
//         let tokens = interpret("1 2 +");
//         assert_eq!(Ok(vec![3.into()]), tokens);
//
//         let tokens = interpret("13.33 243.09 +");
//         assert_eq!(Ok(vec![256.42.into()]), tokens);
//
//         let tokens = interpret("1 [2 3 4] +");
//         assert_eq!(Ok(vec![vec![3, 4, 5].into()]), tokens);
//
//         let tokens = interpret("[5 7 9] 4 +");
//         assert_eq!(Ok(vec![vec![9, 11, 13].into()]), tokens);
//
//         let tokens = interpret("[7 4 1] [6 2 5] +");
//         assert_eq!(Ok(vec![vec![13, 6, 6].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_addition_on_characters() {
//         let tokens = interpret("'a' 2 +");
//         assert_eq!(Ok(vec!['c'.into()]), tokens);
//
//         let tokens = interpret("['c' 'e' 'h'] 2 +");
//         assert_eq!(Ok(vec![vec!['e', 'g', 'j'].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_subtraction_on_numbers() {
//         let tokens = interpret("4 2 -");
//         assert_eq!(Ok(vec![2.into()]), tokens);
//
//         let tokens = interpret("0.3 9.0 -");
//         assert_eq!(Ok(vec![(-8.7).into()]), tokens);
//
//         let tokens = interpret("1 [2 3 4] -");
//         assert_eq!(Ok(vec![vec![-1, -2, -3].into()]), tokens);
//
//         let tokens = interpret("[5 7 9] 4 -");
//         assert_eq!(Ok(vec![vec![1, 3, 5].into()]), tokens);
//
//         let tokens = interpret("[7 4 1] [6 2 5] -");
//         assert_eq!(Ok(vec![vec![1, 2, -4].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_subtraction_on_characters() {
//         let tokens = interpret("'d' 2 -");
//         assert_eq!(Ok(vec!['b'.into()]), tokens);
//
//         let tokens = interpret("['c' 'e' 'h'] 2 -");
//         assert_eq!(Ok(vec![vec!['a', 'c', 'f'].into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_dup() {
//         let tokens = interpret("3 dup");
//         assert_eq!(Ok(vec![3.into(), 3.into()]), tokens);
//
//         let tokens = interpret("2 3 dup");
//         assert_eq!(Ok(vec![2.into(), 3.into(), 3.into()].into()), tokens);
//     }
//
//     #[test]
//     fn test_interpret_swap() {
//         let tokens = interpret("2 3 swap");
//         assert_eq!(Ok(vec![3.into(), 2.into()]), tokens);
//
//         let tokens = interpret("2 3 4 swap");
//         assert_eq!(Ok(vec![2.into(), 4.into(), 3.into()]), tokens);
//     }
//
//     #[test]
//     fn test_interpret_split() {
//         let tokens = interpret(r#""one\ntwo\nthree" '\n'"#);
//         assert_eq!(Ok(vec!["one ".into(), "two".into(), "three".into()]), tokens);
//
//         let tokens = interpret(r#""hi the-re"" "the" split"#);
//         assert_eq!(Ok(vec!["hi ".into(), "-re".into()]), tokens);
//     }
// }
//
