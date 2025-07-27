use crate::interpreter;
use crate::parser::{parse, Node, ParseError};
use crate::scanner::Token;

#[derive(Debug, Clone)]
pub enum InterpretError {
    ParseError(ParseError),
    OperatorIsNotProcedure(Token)
}
struct Interpreter {
    nodes: Vec<Node>,
    errors: Vec<InterpretError>
}

impl Interpreter {
    fn new(nodes: Vec<Node>, parse_errors: Vec<ParseError>) -> Self {
        let errors = parse_errors
            .into_iter()
            .map(InterpretError::ParseError)
            .collect();

        Self { nodes, errors }
    }

    pub fn interpret(&mut self) -> (Vec<i32>, Vec<InterpretError>) {
        panic!("TODO")
    }
}

pub fn interpret(source_code: &str) -> (Vec<i32>, Vec<InterpretError>) {
    let (nodes, errors) = parse(source_code);

    let mut interpreter = Interpreter::new(nodes, errors);
    interpreter.interpret()
}
