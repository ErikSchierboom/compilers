use crate::lexer::{tokenize, LexError, Token, TokenKind};
use crate::location::Span;
use crate::parser::ParseErrorKind::Lex;
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedIdentifier(String),
    ExpectedIdentifier,
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    location: Span,
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        let location = value.location.clone();
        Self { kind: Lex(value), location }
    }
}

#[derive(Clone, Debug)]
pub enum BuiltinKind {
    Dup,
    Drop,
    Swap,
    Over,
}

impl TryFrom<&str> for BuiltinKind {
    type Error = ParseErrorKind;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "dup" => Ok(BuiltinKind::Dup),
            "drop" => Ok(BuiltinKind::Drop),
            "swap" => Ok(BuiltinKind::Swap),
            "over" => Ok(BuiltinKind::Over),
            _ => Err(ParseErrorKind::UnexpectedIdentifier(value.to_string()))
        }
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, location: Span },
    Quote { name: String, location: Span },
    Builtin { kind: BuiltinKind, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { words: Vec<Word>, location: Span },

    // Binary operators
    Add { location: Span },
    Mul { location: Span },

    // Memory operators
    Read { variable: Option<String>, location: Span },
    Write { variable: Option<String>, location: Span },
    Execute { variable: Option<String>, location: Span },
}

impl Word {
    fn location(&self) -> &Span {
        match self {
            Word::Int { location, .. } |
            Word::Quote { location, .. } |
            Word::Builtin { location, .. } |
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

    fn lexeme(&self, location: &Span) -> &'a str {
        &self.code[location.start..location.end]
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
                    Some(Token { kind: TokenKind::Identifier, location }) => {
                        let name = self.lexeme(&location).into();
                        Ok(Word::Quote { name, location })
                    }
                    Some(token) => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location: token.location }),
                    None => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location }),
                }
            }
            TokenKind::Identifier => {
                match BuiltinKind::try_from(self.lexeme(&location)) {
                    Ok(builtin_kind) => Ok(Word::Builtin { kind: builtin_kind, location }),
                    Err(parse_error_kind) => Err(ParseError { kind: parse_error_kind, location })
                }
            }
            TokenKind::Add => Ok(Word::Add { location }),
            TokenKind::Mul => Ok(Word::Mul { location }),

            TokenKind::Read => {
                let variable = self.parse_variable();
                Ok(Word::Read { variable, location })
            }
            TokenKind::Write => {
                let variable = self.parse_variable();
                Ok(Word::Write { variable, location })
            }
            TokenKind::Execute => {
                let variable = self.parse_variable();
                Ok(Word::Execute { variable, location })
            }

            TokenKind::OpenBracket => self.parse_array(location),
            TokenKind::OpenParen => self.parse_block(location),

            _ => Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind.clone()), location })
        };

        Some(result)
    }

    fn parse_variable(&mut self) -> Option<String> {
        self.tokens
            .next_if(|token| token.kind == TokenKind::Identifier)
            .map(|token| self.lexeme(&token.location).into())
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
                Some(Err(error)) => return Err(error.into()),
                None => {
                    let last_location = words.last().map(|word| word.location()).unwrap_or(&start).clone();
                    let location = Span { start: last_location.end + 1, end: last_location.end + 2 };
                    return Err(ParseError { kind: ParseErrorKind::ExpectedToken(end_delimiter), location });
                }
            }
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    let tokens = tokenize(code)?;
    let parser = Parser::new(code, tokens.into_iter());
    parser.parse()
}
