use std::iter::Peekable;
use crate::lexer::Token;
use crate::parser::{BinaryOperator, Expression, ParseError, UnaryOperator};

struct RecursiveDescentParser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> RecursiveDescentParser<I> {
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

        while let Some(operator) = self.tokens.next_if(|token| matches!(token, Token::Plus | Token::Minus)) {
            let operator: BinaryOperator = operator.into();
            let right = self.parse_factor()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_unary()?;

        while let Some(operator) = self.tokens.next_if(|token| matches!(token, Token::Star | Token::Slash)) {
            let operator: BinaryOperator = operator.into();
            let right = self.parse_unary()?;
            expr = Expression::Binary(Box::new(expr), operator, Box::new(right))
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expression, ParseError> {
        if let Some(operator) = self.tokens.next_if(|token| matches!(token, Token::Plus | Token::Minus)) {
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
            Some(Token::Number(i)) => Ok(Expression::Number(i)),
            Some(Token::LParen) => {
                let expr = self.parse()?;
                if self.tokens.next_if(|token| matches!(token, Token::RParen)).is_some() {
                    Ok(Expression::Grouping(Box::new(expr)))
                } else {
                    Err(ParseError::ExpectedToken(Token::RParen))
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken(token))
        }
    }
}

pub fn recursive_descent_parse(tokens: Vec<Token>) -> Result<Expression, ParseError> {
    let mut parser = RecursiveDescentParser::new(tokens.into_iter());
    parser.parse()
}
