use crate::scanner::{scan, Token, ScanError};

#[derive(Debug, Clone)]
pub enum ParseError {
    ScanError(ScanError),
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
        let token = self.tokens.get(self.current)?;
        self.current += 1;
        match token {
            Token::LParen => Some(self.list()),
            Token::RParen => Some(Err(ParseError::UnexpectedToken(Token::RParen))),
            Token::Identifier(identifier) => Some(Ok(Node::Symbol(identifier.to_string()))),
            Token::Number(value) => Some(Ok(Node::Number(value.clone())))
        }
    }

    fn list(&mut self) -> Result<Node, ParseError> {
        let elements: Vec<Node> = Vec::new();

        // while let Some(Ok(element))

        panic!("help")
    }
}

pub fn parse(source_code: &str) -> (Vec<Node>, Vec<ParseError>) {
    let (tokens, scan_errors) = scan(source_code);
    // TODO: handle scan errors
    let mut parser = Parser::new(tokens);
    parser.parse()
}