use std::iter::Peekable;
use crate::lexer::{tokenize, TokenKind};
use crate::parser::{BinaryOperator, Expression, ParseError, UnaryOperator};

struct RecursiveDescentParser<I: Iterator<Item =TokenKind>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item =TokenKind>> RecursiveDescentParser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        if self.tokens.peek().is_some() {
            self.parse_term()
        } else {
            Err(ParseError::UnexpectedEndOfFile)
        }
    }

    fn parse_term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_factor()?;

        while let Some(operator) = self.tokens.next_if(|token| matches!(token, TokenKind::Plus | TokenKind::Minus)) {
            let operator: BinaryOperator = operator.into();
            let right = self.parse_factor()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary()?;

        while let Some(operator) = self.tokens.next_if(|token| matches!(token, TokenKind::Star | TokenKind::Slash)) {
            let operator: BinaryOperator = operator.into();
            let right = self.parse_unary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if let Some(operator) = self.tokens.next_if(|token| matches!(token, TokenKind::Plus | TokenKind::Minus)) {
            let operator: UnaryOperator = operator.into();
            let right = self.parse_unary()?;
            Ok(Expression::Unary(Box::new(right), operator))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Expression, ParseError> {
        match self.tokens.next() {
            None => Err(ParseError::UnexpectedEndOfFile),
            Some(TokenKind::Number(i)) => Ok(Expression::Number(i)),
            Some(TokenKind::LParen) => {
                let expr = self.parse()?;
                if self.tokens.next_if(|token| matches!(token, TokenKind::RParen)).is_some() {
                    Ok(Expression::Grouping(Box::new(expr)))
                } else {
                    Err(ParseError::ExpectedToken(TokenKind::RParen))
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken(token))
        }
    }
}

pub fn recursive_descent_parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => RecursiveDescentParser::new(tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}
