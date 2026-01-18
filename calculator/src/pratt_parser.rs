use crate::lexer::{tokenize, Token};
use crate::parser::{Expression, ParseError};
use std::collections::HashMap;
use std::iter::Peekable;

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy)]
enum Precedence {
    None,
    Term,
    Factor,
    Unary,
    Call,
}

type PrefixParseFn<I> = for<'a> fn(&'a mut PrattParser<I>, Token) -> Result<Expression, ParseError>;
type InfixParseFn<I> = for<'a> fn(&'a mut PrattParser<I>, Expression, Token) -> Result<Expression, ParseError>;

struct ParseRule<I: Iterator<Item = Token>> {
    prefix: Option<PrefixParseFn<I>>,
    infix: Option<InfixParseFn<I>>,
    precedence: Precedence,
}

struct PrattParser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
    rules: HashMap<Token, ParseRule<I>>,
}

impl<I: Iterator<Item = Token>> PrattParser<I> {
    pub fn new(tokens: I) -> Self {
        let rules = HashMap::from([
            (Token::Plus, ParseRule {
                prefix: Some(Self::parse_unary),
                infix: Some(Self::parse_binary),
                precedence: Precedence::Term,
            }),
            (Token::Minus, ParseRule {
                prefix: Some(Self::parse_unary),
                infix: Some(Self::parse_binary),
                precedence: Precedence::Term,
            }),
            (Token::Star, ParseRule {
                prefix: None,
                infix: Some(Self::parse_binary),
                precedence: Precedence::Factor,
            }),
            (Token::Slash, ParseRule {
                prefix: None,
                infix: Some(Self::parse_binary),
                precedence: Precedence::Factor,
            }),
            (Token::LParen, ParseRule {
                prefix: Some(Self::parse_grouping),
                infix: None,
                precedence: Precedence::Call,
            }),
        ]);

        Self {
            tokens: tokens.peekable(),
            rules,
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        self.parse_precedence(Precedence::None)
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let token = self.tokens.next().ok_or(ParseError::UnexpectedEndOfFile)?;

        // TODO: fix this ugly hack
        let prefix_fn = match &token {
            Token::Number(_) => Self::parse_number,
            _ => self.rules.get(&token)
                .and_then(|r| r.prefix)
                .ok_or(ParseError::ExpectedExpression)?,
        };

        let mut left = prefix_fn(self, token)?;

        while let Some(next_token) = self.tokens.peek().cloned() {
            let rule = match self.rules.get(&next_token) {
                Some(r) if r.infix.is_some() && precedence < r.precedence => r,
                _ => break,
            };


            let infix_fn = rule.infix.unwrap();
            let op = self.tokens.next().unwrap();
            left = infix_fn(self, left, op)?;
        }

        Ok(left)
    }

    fn parse_unary(&mut self, token: Token) -> Result<Expression, ParseError> {
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expression::Unary(Box::new(right), token.into()))
    }

    fn parse_binary(&mut self, left: Expression, token: Token) -> Result<Expression, ParseError> {
        let infix_rule = self.rules.get(&token).unwrap();
        let right = self.parse_precedence(infix_rule.precedence)?;
        Ok(Expression::Binary(Box::new(left), token.into(), Box::new(right)))
    }

    fn parse_grouping(&mut self, _token: Token) -> Result<Expression, ParseError> {
        let expr = self.parse_precedence(Precedence::None)?;
        self.tokens.next_if_eq(&Token::RParen).ok_or(ParseError::ExpectedToken(Token::RParen))?;
        Ok(expr)
    }

    fn parse_number(&mut self, token: Token) -> Result<Expression, ParseError> {
        match token {
            Token::Number(i) => Ok(Expression::Number(i)),
            _ => unreachable!(),
        }
    }
}

pub fn pratt_parse(code: &str) -> Result<Expression, ParseError> {
    match tokenize(code) {
        Ok(tokens) => PrattParser::new(tokens.into_iter()).parse(),
        Err(error) => Err(ParseError::Lexical(error)),
    }
}