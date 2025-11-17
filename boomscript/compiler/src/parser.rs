use crate::lexer::{tokenize, LexError, Token};
// use crate::parser::Expression::Pipeline;
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
        value: Vec<Word>,
    },
    Expression(Vec<Word>),
}

#[derive(Debug)]
pub enum Word {
    Number(i64),
    Char(char),
    String(String),
    Identifier(String),
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
            statements.push(self.parse_statement()?)
        }

        Ok(statements)
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        while self.matches(&Token::Newline) {}

        if self.matches(&Token::Let) {
            self.parse_assignment_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    fn parse_assignment_statement(&mut self) -> Result<Statement, ParseError> {
        match self.parse_word()? {
            Word::Identifier(name) => {
                todo!()
            }
            _ => return Err(ParseError::ExpectedIdentifier)
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        todo!()
    }

    pub fn parse_word(&mut self) -> Result<Word, ParseError> {
        let word = match self.advance()? {
            Token::Number(number) => Word::Number(number),
            Token::Char(c) => Word::Char(c),
            Token::String(string) => Word::String(string),
            Token::Identifier(name) => Word::Identifier(name),
            token => return Err(ParseError::UnexpectedToken(token))
        };

        Ok(word)
    }

    fn matches(&mut self, expected: &Token) -> bool {
        self.tokens.next_if_eq(&expected).is_some()
    }

    fn matches_any(&mut self, expected: &[Token]) -> Option<Token> {
        expected.into_iter().find_map(|expected_token| self.tokens.next_if_eq(&expected_token))
    }

    fn advance(&mut self) -> Result<Token, ParseError> {
        match self.tokens.next() {
            None => Err(ParseError::UnexpectedEndOfFile),
            Some(token) => Ok(token)
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParseError> {
        match self.tokens.next_if_eq(token) {
            None => Err(ParseError::ExpectedToken(token.clone())),
            Some(_) => Ok(())
        }
    }
}

pub fn parse(source_code: &str) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
