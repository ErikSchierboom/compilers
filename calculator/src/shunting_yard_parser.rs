use std::collections::VecDeque;
use std::iter::Peekable;
use crate::lexer::{tokenize, Token};
use crate::parser::{BinaryOperator, Expression, ParseError, UnaryOperator};

struct ShuntingYardParser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> ShuntingYardParser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        let mut output = VecDeque::new();
        let mut operators = Vec::new();

        while let Some(token) = self.tokens.next() {
            match &token {
                Token::Number(_) => output.push_back(token),
                Token::Plus | Token::Minus | Token::Star | Token::Slash => {
                    while let Some(operator) = operators.pop_if(|top| !matches!(top, Token::LParen) && top.precedence() > token.precedence()) {
                        output.push_back(operator)
                    }

                    operators.push(token)
                }
                Token::LParen => operators.push(token),
                Token::RParen => {
                    while let Some(operator) = operators.pop_if(|top| !matches!(top, Token::LParen)) {
                        output.push_back(operator)
                    }

                    if let None = operators.pop_if(|token| matches!(token, Token::LParen)) {
                        return Err(ParseError::ExpectedToken(Token::LParen))
                    }
                }
            }
        }

        while let Some(operator) = operators.pop() {
            if matches!(operator, Token::LParen) {
                return Err(ParseError::UnexpectedToken(operator))
            }

            output.push_back(operator)
        }

        let mut stack = Vec::new();

        while let Some(token) = output.pop_front() {
            match token {
                Token::Number(i) => stack.push(Expression::Number(i)),
                Token::Plus => {
                    let right = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    let left = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                stack.push(Expression::Binary(Box::new(left), BinaryOperator::Add, Box::new(right)))
                }
                Token::Minus => {
                    let right = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    let left = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    stack.push(Expression::Binary(Box::new(left), BinaryOperator::Sub, Box::new(right)))
                }
                Token::Star => {
                    let right = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    let left = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    stack.push(Expression::Binary(Box::new(left), BinaryOperator::Mul, Box::new(right)))
                }
                Token::Slash => {
                    let right = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    let left = stack.pop().ok_or_else(|| ParseError::MissingOperand)?;
                    stack.push(Expression::Binary(Box::new(left), BinaryOperator::Div, Box::new(right)))
                }
                Token::LParen => todo!(),
                Token::RParen => todo!(),
            }
        }

        stack.pop().ok_or_else(|| ParseError::UnexpectedEndOfFile)
    }
}

pub fn shunting_yard_parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => ShuntingYardParser::new(tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}
