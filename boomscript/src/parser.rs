use crate::lexer::{tokenize, LexError, Token};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    UnexpectedEndOfFile,
    ExpectedToken(Token),
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int(i64),
    Quote(String),

    // Composite
    Block(Vec<Word>),
    Array(Vec<Word>),

    // Binary operators
    Add,
    Mul,

    // Stack operators
    Dup,
    Drop,
    Swap,
    Over,

    // Memory operators
    Read(Option<String>),
    Write(Option<String>),
    Execute(Option<String>),
}

struct Parser<T: Iterator<Item=Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item=Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        Self { tokens: tokens.peekable() }
    }

    fn parse(&mut self) -> Result<Vec<Word>, ParseError> {
        let mut words = Vec::new();

        while let Some(token) = self.tokens.next() {
            match token {
                Token::Int(i) => words.push(Word::Int(i)),
                Token::Quote(word) => words.push(Word::Quote(word)),
                Token::Add => words.push(Word::Add),
                Token::Mul => words.push(Word::Mul),
                Token::Dup => words.push(Word::Dup),
                Token::Drop => words.push(Word::Drop),
                Token::Swap => words.push(Word::Swap),
                Token::Over => words.push(Word::Over),
                Token::Read(identifier) => words.push(Word::Read(identifier)),
                Token::Write(identifier) => words.push(Word::Write(identifier)),
                Token::Execute(identifier) => words.push(Word::Execute(identifier)),
                Token::OpenBracket => words.push(self.parse_array()?),
                Token::CloseBracket => return Err(ParseError::UnexpectedToken(token)),
                Token::OpenParen => words.push(self.parse_block()?),
                Token::CloseParen => return Err(ParseError::UnexpectedToken(token)),
            }
        }

        Ok(words)
    }

    fn parse_block(&mut self) -> Result<Word, ParseError> {
        let mut words = Vec::new();

        loop {
            match self.tokens.peek() {
                None => return Err(ParseError::ExpectedToken(Token::CloseParen)),
                Some(Token::CloseParen) => {
                    self.tokens.next();
                    break;
                }
                Some(_) => words.push(self.parse_word()?)
            }
        }

        Ok(Word::Block(words))
    }

    fn parse_array(&mut self) -> Result<Word, ParseError> {
        let mut words = Vec::new();

        loop {
            match self.tokens.peek() {
                None => return Err(ParseError::ExpectedToken(Token::CloseBracket)),
                Some(Token::CloseBracket) => {
                    self.tokens.next();
                    break;
                }
                Some(_) => words.push(self.parse_word()?)
            }
        }

        Ok(Word::Array(words))
    }

    fn parse_word(&mut self) -> Result<Word, ParseError> {
        // TODO: remove duplication
        match self.tokens.next() {
            Some(token) => {
                match token {
                    Token::Int(i) => Ok(Word::Int(i)),

                    Token::Quote(word) => Ok(Word::Quote(word)),
                    Token::Add => Ok(Word::Add),
                    Token::Mul => Ok(Word::Mul),
                    Token::Dup => Ok(Word::Dup),
                    Token::Drop => Ok(Word::Drop),
                    Token::Swap => Ok(Word::Swap),
                    Token::Over => Ok(Word::Over),
                    Token::Read(identifier) => Ok(Word::Read(identifier)),
                    Token::Write(identifier) => Ok(Word::Write(identifier)),
                    Token::Execute(identifier) => Ok(Word::Execute(identifier)),
                    Token::OpenBracket => todo!("parse block"),
                    Token::CloseBracket => Err(ParseError::UnexpectedToken(token)),
                    Token::OpenParen => todo!("parse block"),
                    Token::CloseParen => Err(ParseError::UnexpectedToken(token)),
                }
            }
            _ => Err(ParseError::UnexpectedEndOfFile)
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    let tokens = tokenize(code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
