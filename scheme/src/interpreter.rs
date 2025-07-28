use crate::parser::{parse, Expression};
use crate::scanner::SyntaxError;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum RuntimeError {
    MissingFunction,
    ExpectedInteger,
    UnknownFunction(String)
}

struct Interpreter {
    nodes: Vec<Expression>,
    syntax_errors: Vec<SyntaxError>,
    runtime_errors: Vec<RuntimeError>,
    stack: Vec<Expression>,
    current: usize,
}

pub enum Variable {
    Expression(Expression),
    BuiltInFunction(Box<dyn Fn(Vec<Expression>) -> Result<Expression, RuntimeError>>)
}


pub struct Env {
    parent: Option<Box<Env>>,
    variables: HashMap<String, Variable>
}

impl Env {
    pub fn default() -> Self {
        Self { parent: None, variables: HashMap::from([
                ("+".to_string(), Variable::BuiltInFunction(Box::new(|args: Vec<Expression>| {
                    args.iter().fold(Ok(Expression::Integer(0)), |acc_result, arg| {
                       match (acc_result, arg) {
                           (Ok(Expression::Integer(acc)), Expression::Integer(i)) => Ok(Expression::Integer(acc + i)),
                           (Ok(_), _) => Err(RuntimeError::ExpectedInteger),
                           (result, _) => result
                       }
                    })
                }))),
                ("*".to_string(), Variable::BuiltInFunction(Box::new(|args: Vec<Expression>| {
                    args.iter().fold(Ok(Expression::Integer(1)), |acc_result, arg| {
                        match (acc_result, arg) {
                            (Ok(Expression::Integer(acc)), Expression::Integer(i)) => Ok(Expression::Integer(acc * i)),
                            (Ok(_), _) => Err(RuntimeError::ExpectedInteger),
                            (result, _) => result
                        }
                    })
                })))
            ]) }
    }
}

impl Interpreter {
    fn new(nodes: Vec<Expression>, syntax_errors: Vec<SyntaxError>) -> Self {
        Self { nodes, syntax_errors, runtime_errors: Vec::new(), stack: Vec::new(), current: 0 }
    }

    pub fn interpret(&mut self) -> (Vec<Expression>, Vec<SyntaxError>, Vec<RuntimeError>) {
        if self.syntax_errors.len() > 0 {
            return (self.stack.clone(), self.syntax_errors.clone(), self.runtime_errors.clone())
        }

        let env = Env::default();
        
        while let Some(node) = self.nodes.pop() {
            let new_node = self.interpret_node(&node, &env);
            self.stack.push(new_node)
        }

        (self.stack.clone(), self.syntax_errors.clone(), self.runtime_errors.clone())
    }

    pub fn interpret_node(&mut self, node: &Expression, env: &Env) -> Expression {
        match node {
            Expression::Symbol(_) |
            Expression::Integer(_) => node.clone(),
            Expression::List(elements) => {
                match &elements[..] {
                    [] => {
                        self.runtime_errors.push(RuntimeError::MissingFunction);
                        node.clone()
                    },
                    [Expression::Symbol(name), args @ ..] => {
                        match env.variables.get(name) {
                            None => {
                                self.runtime_errors.push(RuntimeError::UnknownFunction(name.to_string()));
                                node.clone()
                            },
                            Some(Variable::Expression(expression)) => expression.clone(),
                            Some(Variable::BuiltInFunction(function)) => {
                                let evaluated_args = args.into_iter().map(|arg| self.interpret_node(arg, env)).collect();
                                match function(evaluated_args) {
                                    Ok(result) => result,
                                    Err(error) => {
                                        self.runtime_errors.push(error);
                                        node.clone()
                                    }
                                }
                            }
                        }


                    },
                _ => node.clone()
                }
            }
        }
    }
}

pub fn interpret(source_code: &str) -> (Vec<Expression>, Vec<SyntaxError>, Vec<RuntimeError>) {
    let (nodes, errors) = parse(source_code);

    let mut interpreter = Interpreter::new(nodes, errors);
    interpreter.interpret()
}
