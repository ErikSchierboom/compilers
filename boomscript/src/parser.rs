use crate::lexer::{tokenize, LexError, Token, TokenKind};
use crate::location::Span;
use crate::parser::ParseErrorKind::Lex;
use std::iter::Peekable;

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    location: Span,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedIdentifier(String),
    ExpectedIdentifier,
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        let location = value.location.clone();
        Self { kind: Lex(value), location }
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, location: Span },
    Quote { name: String, location: Span },
    Word { name: String, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { words: Vec<Word>, location: Span },

    // Binary operators
    Add { location: Span },
    Mul { location: Span },

    // Memory operators
    Read { location: Span },
    Write { location: Span },
    Execute { location: Span },
}

impl Word {
    fn location(&self) -> &Span {
        match self {
            Word::Int { location, .. } |
            Word::Quote { location, .. } |
            Word::Word { location, .. } |
            Word::Block { location, .. } |
            Word::Array { location, .. } |
            Word::Add { location, .. } |
            Word::Mul { location, .. } |
            Word::Read { location, .. } |
            Word::Write { location, .. } |
            Word::Execute { location, .. } => location
        }
    }
}

struct Parser<'a, T: Iterator<Item=Token>> {
    code: &'a str,
    tokens: Peekable<T>,
    words: Vec<Word>,
}

impl<'a, T: Iterator<Item=Token>> Parser<'a, T> {
    fn new(code: &'a str, tokens: T) -> Self {
        Self { code, tokens: tokens.peekable(), words: Vec::new() }
    }

    fn parse(mut self) -> Result<Vec<Word>, ParseError> {
        while let Some(word) = self.parse_word() {
            self.words.push(word?);
        }

        Ok(self.words)
    }

    fn parse_word(&mut self) -> Option<Result<Word, ParseError>> {
        let token = self.tokens.next()?;
        let location = token.location.clone();

        let result = match &token.kind {
            TokenKind::Int => Ok(Word::Int { value: self.lexeme(&location).parse().unwrap(), location }),
            TokenKind::Quote => {
                match self.tokens.next() {
                    Some(Token { kind: TokenKind::Word, location }) => {
                        let name = self.lexeme(&location).into();
                        Ok(Word::Quote { name, location })
                    }
                    Some(token) => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location: token.location }),
                    None => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location }),
                }
            }
            TokenKind::Word => {
                let name = self.lexeme(&location).into();
                Ok(Word::Word { name, location })
            }

            TokenKind::Add => Ok(Word::Add { location }),
            TokenKind::Mul => Ok(Word::Mul { location }),

            TokenKind::Read => Ok(Word::Read { location }),
            TokenKind::Write => Ok(Word::Write { location }),
            TokenKind::Execute => Ok(Word::Execute { location }),

            TokenKind::OpenBracket => self.parse_array(location),
            TokenKind::OpenParen => self.parse_block(location),

            _ => Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind.clone()), location })
        };

        Some(result)
    }

    fn parse_block(&mut self, start: Span) -> Result<Word, ParseError> {
        self.parse_delimited(TokenKind::CloseParen, start, |words, location| Word::Block { words, location })
    }

    fn parse_array(&mut self, start: Span) -> Result<Word, ParseError> {
        self.parse_delimited(TokenKind::CloseBracket, start, |words, location| Word::Array { words, location })
    }

    fn parse_delimited(&mut self, end_delimiter: TokenKind, start: Span, constructor: impl FnOnce(Vec<Word>, Span) -> Word) -> Result<Word, ParseError> {
        let mut words = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.kind == end_delimiter) {
                let location = start.merge(&token.location);
                return Ok(constructor(words, location));
            }

            match self.parse_word() {
                Some(Ok(word)) => words.push(word),
                Some(Err(error)) => return Err(error),
                None => {
                    let last_location = words.last().map(Word::location).unwrap_or(&start);
                    let location = Span { start: last_location.end + 1, end: last_location.end + 2 };
                    return Err(ParseError { kind: ParseErrorKind::ExpectedToken(end_delimiter), location });
                }
            }
        }
    }

    fn lexeme(&self, location: &Span) -> &'a str {
        &self.code[location.start..location.end]
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    let tokens = tokenize(code)?;
    let parser = Parser::new(code, tokens.into_iter());
    parser.parse()
}
