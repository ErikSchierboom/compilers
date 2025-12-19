use crate::lexer::{tokenize, Token};
use crate::parser::UntypedExpression::BinaryOperation;

#[derive(Debug)]
pub enum UntypedExpression {
    Int(i64),
    Float(f64),
    Variable(String),
    BinaryOperation(Box<UntypedExpression>, BinaryOperator, Box<UntypedExpression>),
    Let(String, Box<UntypedExpression>),
}

// TODO: consider if binary operators should just be regular calls
#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Mul,
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(code: &str) -> Self {
        Self { tokens: tokenize(code), current: 0 }
    }

    fn parse(&mut self) -> Vec<UntypedExpression> {
        let mut statements = Vec::new();

        while self.current < self.tokens.len() {
            statements.push(self.parse_statement())
        }

        statements
    }

    fn parse_statement(&mut self) -> UntypedExpression {
        while self.current < self.tokens.len() && matches!(self.tokens[self.current], Token::Newline) {
            self.advance();
        }

        match self.token() {
            Token::Let => self.parse_assignment_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_assignment_statement(&mut self) -> UntypedExpression {
        self.advance();

        match self.token() {
            Token::Variable(name) => {
                self.advance();
                match self.token() {
                    Token::Equal => {
                        self.advance();
                        let value = self.parse_expression();
                        UntypedExpression::Let(name.clone(), Box::new(value))
                    }
                    _ => panic!("expected equal")
                }
            }
            _ => panic!("expected identifier")
        }
    }

    fn parse_expression_statement(&mut self) -> UntypedExpression {
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> UntypedExpression {
        self.parse_term()
    }

    fn parse_term(&mut self) -> UntypedExpression {
        let mut expr = self.parse_factor();

        while self.current < self.tokens.len() && matches!(self.tokens[self.current], Token::Plus) {
            self.advance();
            let right = self.parse_factor();
            expr = BinaryOperation(Box::new(expr), BinaryOperator::Add, Box::new(right))
        }

        expr
    }

    fn parse_factor(&mut self) -> UntypedExpression {
        let mut expr = self.parse_primary();

        while self.current < self.tokens.len() && matches!(self.tokens[self.current], Token::Star) {
            self.advance();
            let right = self.parse_primary();
            expr = BinaryOperation(Box::new(expr), BinaryOperator::Mul, Box::new(right))
        }

        while self.current < self.tokens.len() && matches!(self.tokens[self.current], Token::Newline) {
            self.advance();
        }

        expr
    }

    // TODO: come up with correct terminology
    fn parse_primary(&mut self) -> UntypedExpression {
        match self.token() {
            Token::Int(i) => {
                self.advance();
                UntypedExpression::Int(i.clone())
            }
            Token::Float(f) => {
                self.advance();
                UntypedExpression::Float(f.clone())
            }
            Token::Variable(name) => {
                self.advance();
                UntypedExpression::Variable(name.clone())
            }
            Token::Newline => {
                self.advance();
                self.parse_primary()
            }
            _ => panic!("unexpected token")
        }
    }

    fn token(&mut self) -> Token {
        self.tokens[self.current].clone()
    }

    fn advance(&mut self) {
        self.current += 1;
    }
}

pub fn parse(code: &str) -> Vec<UntypedExpression> {
    let mut parser = Parser::new(code);
    parser.parse()
}
