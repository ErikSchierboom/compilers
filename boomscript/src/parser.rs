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
    UnexpectedEndOfFile,
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
    Sub { location: Span },
    Mul { location: Span },
    Div { location: Span },

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
            Word::Sub { location, .. } |
            Word::Mul { location, .. } |
            Word::Div { location, .. } |
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
        while self.tokens.peek().is_some() {
            self.parse_word()?
        }

        Ok(self.words)
    }

    fn emit(&mut self, word: Word) {
        self.words.push(word)
    }

    fn parse_word(&mut self) -> Result<(), ParseError> {
        let token = self.tokens.next().ok_or_else(|| Self::error(ParseErrorKind::UnexpectedEndOfFile, Span::EMPTY))?;
        let location = token.location.clone();

        match &token.kind {
            TokenKind::Int => self.emit(Word::Int { value: self.lexeme(&location).parse().unwrap(), location }),
            TokenKind::Quote => {
                match self.tokens.next() {
                    Some(Token { kind: TokenKind::Word, location }) => {
                        let name = self.lexeme(&location).into();
                        self.emit(Word::Quote { name, location })
                    }
                    Some(token) => return Err(Self::error(ParseErrorKind::ExpectedIdentifier, token.location)),
                    None => return Err(Self::error(ParseErrorKind::ExpectedIdentifier, location)),
                }
            }
            TokenKind::Word => {
                let name = self.lexeme(&location).into();
                self.emit(Word::Word { name, location })
            }

            TokenKind::Add => self.emit(Word::Add { location }),
            TokenKind::Sub => self.emit(Word::Sub { location }),
            TokenKind::Mul => self.emit(Word::Mul { location }),
            TokenKind::Div => self.emit(Word::Div { location }),

            TokenKind::Read => self.emit(Word::Read { location }),
            TokenKind::ReadVariable => {
                self.emit(Word::Quote { name: self.lexeme(&location)[1..].into(), location: location.clone() });
                self.emit(Word::Read { location })
            }
            TokenKind::Write => self.emit(Word::Write { location }),
            TokenKind::WriteVariable => {
                self.emit(Word::Quote { name: self.lexeme(&location)[1..].into(), location: location.clone() });
                self.emit(Word::Write { location })
            }
            TokenKind::Execute => self.emit(Word::Execute { location }),
            TokenKind::ExecuteVariable => {
                self.emit(Word::Quote { name: self.lexeme(&location)[1..].into(), location: location.clone() });
                self.emit(Word::Execute { location })
            }

            TokenKind::OpenBracket => self.parse_array(location)?,
            TokenKind::OpenParen => self.parse_block(location)?,

            _ => return Err(Self::error(ParseErrorKind::UnexpectedToken(token.kind.clone()), location))
        };

        Ok(())
    }

    fn parse_block(&mut self, start: Span) -> Result<(), ParseError> {
        let (words, location) = self.parse_delimited(TokenKind::CloseParen, start)?;
        self.emit(Word::Block { words, location });
        Ok(())
    }

    fn parse_array(&mut self, start: Span) -> Result<(), ParseError> {
        let (words, location) = self.parse_delimited(TokenKind::CloseBracket, start)?;
        self.emit(Word::Array { words, location });
        Ok(())
    }

    fn parse_delimited(&mut self, end_delimiter: TokenKind, start: Span) -> Result<(Vec<Word>, Span), ParseError> {
        let num_words_before = self.words.len();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.kind == end_delimiter) {
                let words = self.words.drain(num_words_before..).collect();
                let location = start.merge(&token.location);
                return Ok((words, location));
            }

            self.parse_word()?
        }
    }

    fn lexeme(&self, location: &Span) -> &'a str {
        &self.code[location.start..location.end]
    }

    fn error(kind: ParseErrorKind, location: Span) -> ParseError {
        ParseError { kind, location }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    let tokens = tokenize(code)?;
    let parser = Parser::new(code, tokens.into_iter());
    parser.parse()
}
