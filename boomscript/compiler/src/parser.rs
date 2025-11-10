use crate::lexer::{tokenize, LexError, Token};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
}

#[derive(Debug)]
pub enum Statement {
    Assignment {
        name: String,
        value: Expression,
    },

    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Token),

    Binary {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
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

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        // while let Some(token) = tokens.next() {
        //     statements.push(parse_statement()?);
        // }

        Ok(statements)
    }
}

pub fn parse(source_code: &str) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
