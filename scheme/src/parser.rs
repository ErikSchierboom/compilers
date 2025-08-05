use crate::scanner::{scan, SyntaxError, Token};

#[derive(Debug, Clone)]
pub enum Node {
    Integer(i64),
    Bool(bool),
    Char(char),
    String(String),
    Symbol(String),
    List(Vec<Node>),
}

struct Parser {
    tokens: Vec<Token>,
    nodes: Vec<Node>,
    current: usize
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, nodes: Vec::new(), current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Node>, SyntaxError> {
        while let Some(node_result) = self.parse_node() {
            match node_result {
                Ok(node) => self.nodes.push(node),
                Err(error) => return Err(error)
            }
        }

        Ok(self.nodes.clone())
    }

    fn parse_node(&mut self) -> Option<Result<Node, SyntaxError>> {
        let token = self.advance()?;
        match token {
            Token::OpenParen => Some(self.list()),
            Token::CloseParen => Some(Err(SyntaxError::UnexpectedCharacter(')'))),
            Token::Identifier(identifier) => Some(Ok(Node::Symbol(identifier.to_string()))),
            Token::Integer(i) => Some(Ok(Node::Integer(i.clone()))),
            Token::Bool(bool) => Some(Ok(Node::Bool(bool.clone()))),
            Token::Char(c) => Some(Ok(Node::Char(c.clone()))),
            Token::String(str) => Some(Ok(Node::String(str.clone()))),
        }
    }

    fn list(&mut self) -> Result<Node, SyntaxError> {
        let mut elements: Vec<Node> = Vec::new();

        loop {
            if self.advance_if(|token| matches!(token, Token::CloseParen)).is_some() {
                return Ok(Node::List(elements))
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

pub fn parse(source_code: &str) -> Result<Vec<Node>, SyntaxError> {
    let tokens = scan(source_code)?;
    let mut parser = Parser::new(tokens);
    parser.parse()
}