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
    ExpectedIdentifier,
    UnexpectedEndOfFile,
    UnexpectedToken(TokenKind),
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
    Float { value: f64, location: Span },
    Char { value: char, location: Span },
    String { value: String, location: Span },
    Quote { name: String, location: Span },
    Name { name: String, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { words: Vec<Word>, location: Span },
}

impl Word {
    pub fn location(&self) -> &Span {
        match self {
            Word::Int { location, .. } |
            Word::Float { location, .. } |
            Word::Char { location, .. } |
            Word::String { location, .. } |
            Word::Quote { location, .. } |
            Word::Name { location, .. } |
            Word::Block { location, .. } |
            Word::Array { location, .. } => location,
        }
    }
}

struct Parser<'a, T: Iterator<Item=Token>> {
    code: &'a str,
    tokens: Peekable<T>,
}

impl<'a, T: Iterator<Item=Token>> Parser<'a, T> {
    fn new(code: &'a str, tokens: T) -> Self {
        Self { code, tokens: tokens.peekable() }
    }

    fn parse(mut self) -> Result<Vec<Word>, ParseError> {
        std::iter::from_fn(|| self.parse_word()).collect()
    }

    fn parse_word(&mut self) -> Option<Result<Word, ParseError>> {
        let Token { kind, location } = self.tokens.next()?;

        let result = match kind {
            TokenKind::Int => {
                let value = self.lexeme(&location).parse().unwrap();
                Ok(Word::Int { value, location })
            }
            TokenKind::Float => {
                let value = self.lexeme(&location).parse().unwrap();
                Ok(Word::Float { value, location })
            }
            TokenKind::Char => {
                let value = match &self.lexeme(&location)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                Ok(Word::Char { value, location })
            }
            TokenKind::String => {
                let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Ok(Word::String { value, location })
            }
            TokenKind::Quote => {
                let name = self.lexeme(&location)[1..].into();
                Ok(Word::Quote { name, location })
            }
            TokenKind::Word => {
                let name = self.lexeme(&location).into();
                Ok(Word::Name { name, location })
            }
            TokenKind::OpenBracket => self.parse_array(location),
            TokenKind::OpenParen => self.parse_block(location),
            TokenKind::CloseBracket | TokenKind::CloseParen => Err(Self::error(ParseErrorKind::UnexpectedToken(kind), location)),
        };

        Some(result)
    }

    fn parse_block(&mut self, start: Span) -> Result<Word, ParseError> {
        let (words, location) = self.parse_delimited(TokenKind::CloseParen, start)?;
        Ok(Word::Block { words, location })
    }

    fn parse_array(&mut self, start: Span) -> Result<Word, ParseError> {
        let (words, location) = self.parse_delimited(TokenKind::CloseBracket, start)?;
        Ok(Word::Array { words, location })
    }

    fn parse_delimited(&mut self, end_delimiter: TokenKind, start: Span) -> Result<(Vec<Word>, Span), ParseError> {
        let mut words = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.kind == end_delimiter) {
                let location = start.merge(&token.location);
                return Ok((words, location));
            }

            let word = self.parse_word().ok_or_else(|| Self::error(ParseErrorKind::UnexpectedEndOfFile, Span::EMPTY))??;
            words.push(word)
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
