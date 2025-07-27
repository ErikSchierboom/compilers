use crate::parser::Node::List;
use crate::scanner::{scan, Token, ScanError};

#[derive(Debug, Clone)]
pub enum ParseError {
    ScanError(ScanError),
    ExpectedToken(Token),
    UnexpectedToken(Token)
}

#[derive(Debug, Clone)]
pub enum Node {
    Symbol(String),
    Number(i32),
    List(Vec<Node>),
}

struct Parser {
    tokens: Vec<Token>,
    current: usize
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> (Vec<Node>, Vec<ParseError>) {
        let mut nodes: Vec<Node> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();

        while let Some(node_result) = self.parse_node() {
            match node_result {
                Ok(node) => nodes.push(node),
                Err(error) => errors.push(error)
            }
        }

        (nodes, errors)
    }

    fn parse_node(&mut self) -> Option<Result<Node, ParseError>> {
        match self.advance()? {
            Token::LParen => Some(self.list()),
            Token::RParen => Some(Err(ParseError::UnexpectedToken(Token::RParen))),
            Token::Identifier(identifier) => Some(Ok(Node::Symbol(identifier.to_string()))),
            Token::Number(value) => Some(Ok(Node::Number(value.clone())))
        }
    }

    fn list(&mut self) -> Result<Node, ParseError> {
        let mut elements: Vec<Node> = Vec::new();

        loop {
            if self.advance_if(|token| matches!(token, Token::RParen)).is_some() {
                return Ok(List(elements))
            }

            match self.parse_node() {
                Some(Ok(node)) => elements.push(node),
                Some(Err(error)) => return Err(error),
                None => return Err(ParseError::ExpectedToken(Token::RParen))
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

pub fn parse(source_code: &str) -> (Vec<Node>, Vec<ParseError>) {
    let (tokens, scan_errors) = scan(source_code);

    if scan_errors.len() > 0 {
        (Vec::new(), scan_errors.into_iter().map(ParseError::ScanError).collect())
    } else {
        let mut parser = Parser::new(tokens);
        parser.parse()
    }
}