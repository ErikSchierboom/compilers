use crate::lexer::{LexicalError, TokenKind};

#[derive(Debug)]
pub enum ParseError {
    Lexical(LexicalError),
    UnexpectedEndOfFile,
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    ExpectedExpression,
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

impl From<TokenKind> for UnaryOperator {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Pos,
            TokenKind::Minus => Self::Neg,
            _ => panic!("cannot convert token to unary operator")
        }
    }
}

impl From<TokenKind> for BinaryOperator {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            _ => panic!("cannot convert token to binary operator")
        }
    }
}

pub fn parse(code: &str) -> Result<Expression, ParseError> {
    #[cfg(feature = "parser_recursive_descent")]
    {
        crate::recursive_descent_parser::recursive_descent_parse(code)
    }

    #[cfg(feature = "parser_pratt")]
    {
        crate::pratt_parser::pratt_parse(code)
    }
}
