// use crate::parser::{parse, Node, ParseError};

use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::lex::Token;

// #[derive(Clone, Debug)]
// pub enum RuntimeError {
//     Parse(ParseError)
// }
// 
// impl Display for RuntimeError {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             RuntimeError::Parse(parse_error) => write!(f, "{}", parse_error),
//         }
//     }
// }
// 
// impl Error for RuntimeError {}
// 
// #[derive(Debug)]
// pub enum Value {
//     Number(i64),
//     Character(char),
//     String(String),
//     Array(Vec<Value>)
// }
// 
// type InterpretResult = Result<Vec<Value>, Vec<Spanned<RuntimeError>>>;
// 
// pub struct Interpreter {
//     nodes: Vec<Spanned<Node>>
// }
// 
// impl Interpreter {
//     pub fn new(nodes: Vec<Spanned<Node>>) -> Self {
//         Self { nodes }
//     }
// }
// 
// pub fn interpret(source: &Source) -> InterpretResult {
//     match parse(source) {
//         Ok(nodes) => {
//             Err(vec![])
//             // let mut parser = Parser::new(source, tokens);
//             // parser.parse()
//         }
//         Err(errors) =>
//             Err(errors.into_iter()
//                 .map(|error|error.map_value(|e|RuntimeError::Parse(e.clone())))
//                 .collect())
//     }
// }