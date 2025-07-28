use crate::parser::{parse, Node};
use crate::scanner::{SyntaxError, Token};

#[derive(Debug, Clone)]
pub enum RuntimeError {
    OperatorIsNotProcedure(Token)
}

struct Interpreter {
    nodes: Vec<Node>,
    syntax_errors: Vec<SyntaxError>,
    errors: Vec<RuntimeError>,
    stack: Vec<Node>,
    current: usize,
}

impl Interpreter {
    fn new(nodes: Vec<Node>, syntax_errors: Vec<SyntaxError>) -> Self {
        Self { nodes, errors: Vec::new(), syntax_errors, stack: Vec::new(), current: 0 }
    }

    pub fn interpret(&mut self) -> (Vec<Node>, Vec<RuntimeError>) {
        while let Some(node) = self.nodes.pop() {
            let new_node = self.interpret_node(&node);
            self.stack.push(new_node)
        }

        (self.stack.clone(), self.errors.clone())
    }

    pub fn interpret_node(&mut self, node: &Node) -> Node {
        match node {
            Node::Symbol(symbol) => node.clone(),
            Node::Number(value) => node.clone(),
            Node::List(elements) => {
                match &elements[..] {
                    [] => node.clone(),
                    [Node::Symbol(s)] if s == "+"  || s == "*" || s == "-" || s == "/" => Node::Number(0),
                    [Node::Symbol(s), args @ ..] => {
                        let inner: Vec<Node> = args.into_iter().map(|arg| self.interpret_node(arg)).collect();

                        match s.as_str() {
                            "+" => {
                                let new_value = inner.iter().fold(0, |acc, node| {
                                    match node {
                                        Node::Symbol(_) => panic!("did not expect symbol"),
                                        Node::Number(value) => acc + value,
                                        Node::List(_) => panic!("did not expect list"),
                                    }
                                });
                                Node::Number(new_value)
                            },
                            _ => panic!("unknown operator")
                        }
                    },
                _ => node.clone()
                }
            }
        }
    }
}

pub fn interpret(source_code: &str) -> (Vec<Node>, Vec<RuntimeError>) {
    let (nodes, errors) = parse(source_code);

    let mut interpreter = Interpreter::new(nodes, errors);
    interpreter.interpret()
}
