use crate::lexer::{tokenize, Token, TokenKind};
use crate::parser::{Expression, ParseError};
use std::iter::Peekable;

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy)]
enum Precedence {
    None,
    Term,
    Factor,
    Unary,
    Call,
}

struct PrattParser<'a, I: Iterator<Item =Token>> {
    code: &'a str,
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Token>> PrattParser<'a, I> {
    pub fn new(code: &'a str, tokens: I) -> Self {
        Self {
            code,
            tokens: tokens.peekable(),
        }
    }

    fn get_precedence(kind: &TokenKind) -> Precedence {
        match kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash => Precedence::Factor,
            _ => Precedence::None,
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let token = self.tokens.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        let mut left = self.parse_unary(token);

        while let Some(next_token) = self.tokens.peek() {
            let next_precedence = Self::get_precedence(&next_token.kind);
            if precedence >= next_precedence {
                break;
            }

            let token = self.tokens.next().unwrap();
            left = match token.kind {
                TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
                    self.parse_binary(left, token)?
                }
                _ => break,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self, token: Token) -> Expression {
        match token.kind {
            TokenKind::Number => self.parse_number(token)?,
            TokenKind::Plus | TokenKind::Minus => self.parse_unary(token)?,
            TokenKind::LParen => self.parse_grouping(token)?,
            _ => return Err(ParseError::ExpectedExpression),
        }
    }

    fn parse_unary(&mut self, token: Token) -> Result<Expression, ParseError> {
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expression::Unary(Box::new(right), token.kind.into()))
    }

    fn parse_binary(&mut self, left: Expression, token: Token) -> Result<Expression, ParseError> {
        let precedence = Self::get_precedence(&token.kind);
        let right = self.parse_precedence(precedence)?;
        Ok(Expression::Binary(Box::new(left), token.kind.into(), Box::new(right)))
    }

    fn parse_grouping(&mut self, _token: Token) -> Result<Expression, ParseError> {
        let expr = self.parse_precedence(Precedence::None)?;
        self.tokens.next_if(|token| token.kind == TokenKind::RParen).ok_or(ParseError::ExpectedToken(TokenKind::RParen))?;
        Ok(Expression::Grouping(Box::new(expr)))
    }

    fn parse_number(&mut self, token: Token) -> Result<Expression, ParseError> {
        match token.kind {
            TokenKind::Number => Ok(Expression::Number(self.code[token.span.start..token.span.end].parse().unwrap())),
            _ => unreachable!(),
        }
    }
}

pub fn pratt_parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => PrattParser::new(code, tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}