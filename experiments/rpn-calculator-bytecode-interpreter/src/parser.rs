use crate::parser::Expression::Integer;
use crate::scanner::{ScanError, Scanner, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum ParseError {
    ScanError(ScanError)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i32),
    Add,
    Subtract,
    Multiply,
    Divide
}

pub struct Parser<'a> {
    source_code: &'a str,
    scanner: Scanner<'a>
}

impl<'a> Parser<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self { 
            source_code,
            scanner: Scanner::new(source_code)
        }
    }
    
    pub fn parse(&mut self) -> (Vec<Expression>, Vec<ParseError>) {
        let mut expressions: Vec<Expression> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();

        while let Some(token) = self.scanner.scan_token() {
            match self.parse_expression(token) {
                Ok(expression) => expressions.push(expression),
                Err(error) => errors.push(error)
            }
        }

        (expressions, errors)
    }

    fn parse_expression(&self, token: Token) -> Result<Expression, ParseError> {
        match token.kind {
            TokenKind::Numeric => Ok(Integer(self.source_code[token.start..token.stop].parse::<i32>().unwrap())),
            TokenKind::Plus => Ok(Expression::Add),
            TokenKind::Minus => Ok(Expression::Subtract),
            TokenKind::Star => Ok(Expression::Multiply),
            TokenKind::Slash => Ok(Expression::Divide),
            TokenKind::Error(error) => Err(ParseError::ScanError(error))
        }
    }
}
