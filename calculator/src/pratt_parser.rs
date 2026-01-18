use crate::lexer::{tokenize, Token};
use crate::parser::{BinaryOperator, Expression, ParseError, UnaryOperator};
use std::iter::Peekable;

#[derive(Debug, PartialOrd, PartialEq)]
enum Precedence {
    None,
    Term,
    Factor,
    Unary,
    Call
}

struct PrattParser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> PrattParser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable()
        }
    }

    fn prefix_rule(
        token: &Token,
    ) -> Option<(for<'a> fn(&'a mut PrattParser<I>, Token) -> Result<Expression, ParseError>, Precedence)> {
        match token {
            Token::Number(_) => Some((Self::parse_number, Precedence::None)),
            Token::Plus | Token::Minus => Some((Self::parse_unary, Precedence::Term)),
            Token::LParen => Some((Self::parse_grouping, Precedence::Call)),
            Token::Star | Token::Slash | Token::RParen => None
        }
    }

    fn infix_parse_fn(
        token: &Token,
    ) -> Option<(for<'a> fn(&'a mut PrattParser<I>, Expression, Token, Precedence) -> Result<Expression, ParseError>, Precedence)> {
        match token {
            Token::Plus | Token::Minus => Some((Self::parse_binary, Precedence::Term)),
            Token::Star | Token::Slash => Some((Self::parse_binary, Precedence::Factor)),
            Token::Number(_) | Token::LParen | Token::RParen => None
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let token = self.tokens.next().ok_or_else(|| ParseError::UnexpectedEndOfFile)?;

        let (prefix_fn, _prefix_precedence) = Self::prefix_rule(&token)
            .ok_or_else(|| ParseError::ExpectedExpression)?;

        let mut left = prefix_fn(self, token)?;

        loop {
            // look at next token without consuming it and clone it
            let next_token = {
                match self.tokens.peek() {
                    None => break,
                    Some(t) => t,
                }
            }; // mutable borrow ends here

            if let Some((infix_fn, infix_prec)) = Self::infix_parse_fn(&next_token) {
                // only continue if the next operator has higher precedence than current
                if precedence < infix_prec {
                    // consume the operator token
                    let op = self.tokens.next().unwrap();
                    left = infix_fn(self, left, op, infix_prec)?;
                    continue;
                }
            }

            break;
        }

        Ok(left)
    }

    fn parse_unary(&mut self, token: Token) -> Result<Expression, ParseError> {
        let operator: UnaryOperator = token.into();
        // parse right-hand side with unary precedence
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expression::Unary(Box::new(right), operator))
    }

    fn parse_binary(&mut self, left: Expression, token: Token, precedence: Precedence) -> Result<Expression, ParseError> {
        let operator: BinaryOperator = token.into();
        // parse right-hand side with the operator's precedence
        let right = self.parse_precedence(precedence)?;
        Ok(Expression::Binary(Box::new(left), operator, Box::new(right)))
    }

    fn parse_number(&mut self, token: Token) -> Result<Expression, ParseError> {
        match token {
            Token::Number(i) => Ok(Expression::Number(i)),
            _ => unreachable!()
        }
    }

    fn parse_grouping(&mut self, token: Token) -> Result<Expression, ParseError> {
        let expr = self.parse()?;
        self.tokens.next_if(|next| matches!(next, Token::RParen)).ok_or_else(||ParseError::ExpectedToken(Token::RParen))?;
        Ok(expr)
    }
}

pub fn pratt_parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => PrattParser::new(tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}