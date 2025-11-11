use crate::lexer::{tokenize, LexError, Token};
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
    Variable(Token),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
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

        while self.tokens.peek().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        // TODO: extract helper method
        if self.tokens.next_if_eq(&Token::Let).is_some() {
            self.parse_assignment_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.next() {
            Some(Token::Identifier(name)) => {
                // TODO: extract helper method
                if self.tokens.next_if_eq(&Token::Equal).is_some() {
                    let value = self.parse_expression()?;
                    self.tokens.next_if_eq(&Token::Semicolon).ok_or_else(|| ParseError::ExpectedToken(Token::Semicolon))?;
                    Ok(Statement::Assignment { name, value })
                } else {
                    Err(ParseError::ExpectedToken(Token::Equal))
                }
            }
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expression = self.parse_expression()?;
        self.tokens.next_if_eq(&Token::Semicolon).ok_or_else(|| ParseError::ExpectedToken(Token::Semicolon))?;
        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_term_expression()
    }

    fn parse_term_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_factor_expression()?;

        loop {
            let operator_opt = self.tokens.next();
            match operator_opt {
                Some(Token::Minus) |
                Some(Token::Plus) => {
                    let right = self.parse_factor_expression()?;
                    expr = Expression::Binary { left: Box::new(expr), operator: operator_opt.unwrap(), right: Box::new(right) }
                }
                _ => break
            }
        }

        Ok(expr)
    }

    fn parse_factor_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_call_expression()?;

        loop {
            let operator_opt = self.tokens.next();
            match operator_opt {
                Some(Token::Star) |
                Some(Token::Slash) => {
                    let right = self.parse_call_expression()?;
                    expr = Expression::Binary { left: Box::new(expr), operator: operator_opt.unwrap(), right: Box::new(right) }
                }
                _ => break
            }
        }

        Ok(expr)
    }

    fn parse_call_expression(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_primary_expression()?;

        loop {
            if self.tokens.next_if_eq(&Token::OpenParenthesis).is_some() {
                let mut args: Vec<Expression> = Vec::new();

                loop {
                    if let Some(&Token::OpenParenthesis) = self.tokens.peek() {
                        break;
                    } else {
                        let arg = self.parse_expression()?;
                        args.push(arg);
                        if self.tokens.next_if_eq(&Token::Comma).is_some() {
                            break;
                        }
                    }
                }

                self.tokens.next_if_eq(&Token::CloseParenthesis).ok_or_else(|| ParseError::ExpectedToken(Token::CloseParenthesis))?;

                return Ok(Expression::Call { callee: Box::new(expr), args });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.tokens.next().ok_or_else(|| ParseError::UnexpectedEndOfFile)?;

        match token {
            Token::Number(_) |
            Token::Char(_) |
            Token::String(_) => Ok(Expression::Literal(token)),
            Token::Identifier(_) => Ok(Expression::Variable(token)),
            _ => Err(ParseError::UnexpectedToken(token))
        }
    }
}

pub fn parse(source_code: &str) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
