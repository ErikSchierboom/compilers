use crate::lexer::{tokenize, LexError, Span, Token, TokenKind};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedEndOfFile,
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    location: Span,
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, location: Span },
    Quote { name: String, location: Span },
    Identifier { name: String, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { elements: Vec<Word>, location: Span },

    // Binary operators
    Add { location: Span },
    Mul { location: Span },

    // Stack operators
    Dup { location: Span },
    Drop { location: Span },
    Swap { location: Span },
    Over { location: Span },

    // Memory operators
    Read { variable: Option<String>, location: Span },
    Write { variable: Option<String>, location: Span },
    Execute { variable: Option<String>, location: Span },
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
            let location = token.location;

            match token.kind {
                TokenKind::Int(i) => words.push(Word::Int { value: i, location }),
                TokenKind::Quote(name) => words.push(Word::Quote { name, location }),
                TokenKind::Identifier(name) => words.push(Word::Identifier { name, location }),
                TokenKind::Add => words.push(Word::Add { location }),
                TokenKind::Mul => words.push(Word::Mul { location }),
                TokenKind::Dup => words.push(Word::Dup { location }),
                TokenKind::Drop => words.push(Word::Drop { location }),
                TokenKind::Swap => words.push(Word::Swap { location }),
                TokenKind::Over => words.push(Word::Over { location }),

                // TODO: check if followed by identifier
                TokenKind::Read => words.push(Word::Read { variable: None, location }),
                TokenKind::Write => words.push(Word::Write { variable: None, location }),
                TokenKind::Execute => words.push(Word::Execute { variable: None, location }),

                TokenKind::OpenBracket => words.push(self.parse_array()?),
                TokenKind::CloseBracket => return Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind), location }),
                TokenKind::OpenParen => words.push(self.parse_block()?),
                TokenKind::CloseParen => return Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind), location }),
            }
        }

        Ok(words)
    }

    fn parse_block(&mut self) -> Result<Word, ParseError> {
        let mut words = Vec::new();

        loop {
            match self.tokens.peek() {
                None => return Err(ParseError::ExpectedToken(TokenKind::CloseParen)),
                Some(Token { kind: TokenKind::CloseParen, .. }) => {
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
                None => return Err(ParseError::ExpectedToken(TokenKind::CloseBracket)),
                Some(Token { kind: TokenKind::CloseBracket, .. }) => {
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
                match token.kind {
                    TokenKind::Int(i) => Ok(Word::Int(i)),
                    TokenKind::Quote(word) => Ok(Word::Quote(word)),
                    TokenKind::Identifier(name) => Ok(Word::Identifier(name)),
                    TokenKind::Add => Ok(Word::Add),
                    TokenKind::Mul => Ok(Word::Mul),
                    TokenKind::Dup => Ok(Word::Dup),
                    TokenKind::Drop => Ok(Word::Drop),
                    TokenKind::Swap => Ok(Word::Swap),
                    TokenKind::Over => Ok(Word::Over),
                    // TODO: check if followed by identifier
                    TokenKind::Read => Ok(Word::Read(None)),
                    TokenKind::Write => Ok(Word::Write(None)),
                    TokenKind::Execute => Ok(Word::Execute(None)),
                    TokenKind::OpenBracket => todo!("parse block"),
                    TokenKind::CloseBracket => Err(ParseError::UnexpectedToken(token)),
                    TokenKind::OpenParen => todo!("parse block"),
                    TokenKind::CloseParen => Err(ParseError::UnexpectedToken(token)),
                }
            }
            _ => Err(ParseError::UnexpectedEndOfFile)
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    match tokenize(code) {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens.into_iter());
            parser.parse()
        },
        Err(lex_error) => {
            let location = lex_error.location.clone();
            let kind = ParseErrorKind::Lex(lex_error);
            Err(ParseError { kind, location })
        }
    }
    
}
