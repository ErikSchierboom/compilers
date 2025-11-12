use crate::lexer::{tokenize, LexError, Token};
use crate::parser::Expression::Pipeline;
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
    ExpectedIdentifier,
    UnexpectedEndOfFile,
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
    Pipeline(Vec<Expression>),
}

#[derive(Debug)]
pub enum Literal {
    Number(i64),
    Char(char),
    String(String),
}

struct Parser<T>
where
    T: Iterator<Item=Token>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item=Token>,
{
    fn new(tokens: T) -> Self {
        Self { tokens: tokens.peekable() }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        let mut expressions: Vec<Expression> = Vec::new();

        expressions.push(self.parse_call()?);

        while self.matches(&Token::Pipe) {
            expressions.push(self.parse_call()?);
        }

        Ok(Pipeline(expressions))
    }

    pub fn parse_call(&mut self) -> Result<Expression, ParseError> {
        if let Token::Identifier(function) = self.advance()? {
            let mut arguments: Vec<Expression> = Vec::new();

            loop {
                if self.tokens.peek() == Some(&Token::Pipe) {
                    break;
                }

                arguments.push(self.parse_literal()?)
            }

            Ok(Expression::Call { function, arguments })
        } else {
            self.parse_literal()
        }
    }

    fn parse_literal(&mut self) -> Result<Expression, ParseError> {
        let token = self.advance()?;

        match token {
            Token::Number(number) => Ok(Expression::Literal(Literal::Number(number))),
            Token::Char(char) => Ok(Expression::Literal(Literal::Char(char))),
            Token::String(string) => Ok(Expression::Literal(Literal::String(string))),
            _ => Err(ParseError::UnexpectedToken(token))
        }
    }

    fn matches(&mut self, expected: &Token) -> bool {
        self.tokens.next_if_eq(&expected).is_some()
    }

    fn matches_any(&mut self, expected: &[Token]) -> Option<Token> {
        expected.into_iter().find_map(|expected_token| self.tokens.next_if_eq(&expected_token))
    }

    fn advance(&mut self) -> Result<Token, ParseError> {
        match self.tokens.next() {
            None => Err(ParseError::UnexpectedEndOfFile),
            Some(token) => Ok(token)
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParseError> {
        match self.tokens.next_if_eq(token) {
            None => Err(ParseError::ExpectedToken(token.clone())),
            Some(_) => Ok(())
        }
    }
}

pub fn parse(source_code: &str) -> Result<Expression, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
