use std::iter::Peekable;
use crate::lexer::{tokenize, LexicalError, Token};

#[derive(Debug)]
pub enum ParseError {
    Lexical(LexicalError)
}

pub enum Expression {
    Number(i64),
    Unary(Box<Expression>, UnaryOperator),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Grouping(Box<Expression>)
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub enum UnaryOperator {
    Pos,
    Neg,
}

struct Parser<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn parse(&mut self) -> Result<Expression, ParseError> {
        todo!("")
    }
}

pub fn parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => Parser::new(tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}
