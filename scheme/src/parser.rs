use crate::parser::Node::List;
use crate::scanner::{scan, Token, SyntaxError};

#[derive(Debug, Clone)]
pub enum Node {
    Symbol(String),
    Number(i32),
    List(Vec<Node>),
}

struct Parser {
    tokens: Vec<Token>,
    errors: Vec<SyntaxError>,
    current: usize
}

impl Parser {
    fn new(tokens: Vec<Token>, scan_errors: Vec<SyntaxError>) -> Self {
        Self { tokens, errors: scan_errors, current: 0 }
    }

    pub fn parse(&mut self) -> (Vec<Node>, Vec<SyntaxError>) {
        let mut nodes: Vec<Node> = Vec::new();
  
        while let Some(node_result) = self.parse_node() {
            match node_result {
                Ok(node) => nodes.push(node),
                Err(error) => self.errors.push(error)
            }
        }

        (nodes, self.errors.clone())
    }

    fn parse_node(&mut self) -> Option<Result<Node, SyntaxError>> {
        match self.advance()? {
            Token::LParen => Some(self.list()),
            Token::RParen => Some(Err(SyntaxError::UnexpectedCharacter(')'))),
            Token::Identifier(identifier) => Some(Ok(Node::Symbol(identifier.to_string()))),
            Token::Number(value) => Some(Ok(Node::Number(value.clone())))
        }
    }

    fn list(&mut self) -> Result<Node, SyntaxError> {
        let mut elements: Vec<Node> = Vec::new();

        loop {
            if self.advance_if(|token| matches!(token, Token::RParen)).is_some() {
                return Ok(List(elements))
            }

            match self.parse_node() {
                Some(Ok(node)) => elements.push(node),
                Some(Err(error)) => return Err(error),
                None => return Err(SyntaxError::ExpectedCharacter(')'))
            }
        }
    }

    fn advance(&mut self) -> Option<&Token> {
        self.advance_if(|_| true)
    }

    fn advance_if(&mut self, func: impl FnOnce(&Token) -> bool) -> Option<&Token> {
        self.tokens.get(self.current)
            .filter(|&token| func(token))
            .inspect(|_| self.current += 1)
    }
}

pub fn parse(source_code: &str) -> (Vec<Node>, Vec<SyntaxError>) {
    let (tokens, scan_errors) = scan(source_code);

    let mut parser = Parser::new(tokens, scan_errors);
    parser.parse()
}