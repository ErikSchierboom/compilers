use crate::scanner::{scan, Token};

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

    pub fn parse(&mut self) -> Vec<Node> {
        let mut nodes: Vec<Node> = Vec::new();

        while let Some(token) = self.tokens.get(self.current) {
            match token {
                Token::LParen => {}
                Token::RParen => eprintln!(""),
                Token::Identifier(_) => {}
                Token::Number(_) => {}
            }
        }
        
        nodes
    }
}

pub fn parse(source_code: &str) -> Vec<Node> {
    let tokens = scan(source_code);
    let mut parser = Parser::new(tokens);
    parser.parse()
}