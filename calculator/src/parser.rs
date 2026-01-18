use crate::lexer::{LexicalError, Token};
use crate::recursive_descent_parser::recursive_descent_parse;

#[derive(Debug)]
pub enum ParseError {
    Lexical(LexicalError),
    UnexpectedEndOfFile,
    ExpectedToken(Token),
    UnexpectedToken(Token),
}

#[derive(Debug)]
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

impl From<Token> for UnaryOperator {
    fn from(value: Token) -> Self {
        match value {
            Token::Plus => Self::Pos,
            Token::Minus => Self::Neg,
            _ => panic!("cannot convert token to unary operator")
        }
    }
}

impl From<Token> for BinaryOperator {
    fn from(value: Token) -> Self {
        match value {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            _ => panic!("cannot convert token to binary operator")
        }
    }
}

// TODO: implement Shunting Yard parser
// TODO: implement Pratt parse

pub fn parse(code: &str) -> Result<Expression, ParseError> {
    #[cfg(feature = "parser_recursive_descent")]
    recursive_descent_parse(code)
}
