use std::collections::HashMap;
use crate::parser::{parse, Atom, Expression, Number};
use crate::scanner::{SyntaxError, Token};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    OperatorIsNotProcedure(Token)
}

struct Interpreter {
    nodes: Vec<Expression>,
    syntax_errors: Vec<SyntaxError>,
    errors: Vec<RuntimeError>,
    stack: Vec<Expression>,
    current: usize,
}

pub struct Env {
    parent: Option<Box<Env>>,
    variables: HashMap<String, Expression>
}

// impl Interpreter {
//     fn new(nodes: Vec<Expression>, syntax_errors: Vec<SyntaxError>) -> Self {
//         Self { nodes, errors: Vec::new(), syntax_errors, stack: Vec::new(), current: 0 }
//     }
//
//     pub fn interpret(&mut self) -> (Vec<Expression>, Vec<RuntimeError>) {
//         while let Some(node) = self.nodes.pop() {
//             let new_node = self.interpret_node(&node);
//             self.stack.push(new_node)
//         }
//
//         (self.stack.clone(), self.errors.clone())
//     }
//
//     pub fn interpret_node(&mut self, node: &Expression) -> Expression {
//         match node {
//             Expression::Atom(Atom::Symbol(_)) => node.clone(),
//             Expression::Atom(Atom::Number(_)) => node.clone(),
//             Expression::List(elements) => {
//                 match &elements[..] {
//                     [] => node.clone(),
//                     [Expression::Atom(Atom::Symbol(s))] if s == "+"  || s == "*" || s == "-" || s == "/" => Expression::Number(0),
//                     [Expression::Atom(Atom::Symbol(s)), args @ ..] => {
//                         let inner: Vec<Expression> = args.into_iter().map(|arg| self.interpret_node(arg)).collect();
//
//                         match s.as_str() {
//                             "+" => {
//                                 let new_value = inner.iter().fold(0, |acc, node| {
//                                     match node {
//                                         Expression::Atom(Atom::Symbol(_)) => panic!("did not expect symbol"),
//                                         Expression::Atom(Atom::Number(Number::Integer(value))) => acc + value,
//                                         Expression::List(_) => panic!("did not expect list"),
//                                     }
//                                 });
//                                 Expression::Number(new_value)
//                             },
//                             _ => panic!("unknown operator")
//                         }
//                     },
//                 _ => node.clone()
//                 }
//             }
//         }
//     }
// }
//
// pub fn interpret(source_code: &str) -> (Vec<Expression>, Vec<RuntimeError>) {
//     let (nodes, errors) = parse(source_code);
//
//     let mut interpreter = Interpreter::new(nodes, errors);
//     interpreter.interpret()
// }
