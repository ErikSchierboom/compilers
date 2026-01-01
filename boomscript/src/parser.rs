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

    fn parse(mut self) -> Result<Vec<Word>, Vec<ParseError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(result) = self.parse_word() {
            match result {
                Ok(word) => tokens.push(word),
                Err(parse_error) => errors.push(parse_error)
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn parse_word(&mut self) -> Option<Result<Word, ParseError>> {
        let Token { kind, location } = self.tokens.next()?;

        match kind {
            TokenKind::Int => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Word::Int { value, location }))
            }
            TokenKind::Float => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Word::Float { value, location }))
            }
            TokenKind::Char => {
                let value = match &self.lexeme(&location)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                Some(Ok(Word::Char { value, location }))
            }
            TokenKind::String => {
                let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Some(Ok(Word::String { value, location }))
            }
            TokenKind::Quote => {
                let name = self.lexeme(&location)[1..].into();
                Some(Ok(Word::Quote { name, location }))
            }
            TokenKind::Word => {
                let name = self.lexeme(&location).into();
                Some(Ok(Word::Name { name, location }))
            }
            TokenKind::OpenBracket => {
                match self.parse_delimited(TokenKind::CloseBracket, location) {
                    Ok((words, location)) => Some(Ok(Word::Array { words, location })),
                    Err(err) => Some(Err(err))
                }
            }
            TokenKind::OpenParen => {
                match self.parse_delimited(TokenKind::CloseParen, location) {
                    Ok((words, location)) => Some(Ok(Word::Block { words, location })),
                    Err(err) => Some(Err(err))
                }
            }
            TokenKind::CloseBracket |
            TokenKind::CloseParen => Some(Err(ParseError { kind: ParseErrorKind::UnexpectedToken(kind), location })),
        }
    }

    fn parse_delimited(&mut self, end_delimiter: TokenKind, start: Span) -> Result<(Vec<Word>, Span), ParseError> {
        let mut words = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.kind == end_delimiter) {
                let location = start.merge(&token.location);
                return Ok((words, location));
            }

            let word = self.parse_word().ok_or_else(|| ParseError { kind: ParseErrorKind::UnexpectedEndOfFile, location: Span::EMPTY })??;
            words.push(word)
        }
    }

    fn lexeme(&self, location: &Span) -> &'a str {
        &self.code[location.start..location.end]
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, Vec<ParseError>> {
    match tokenize(code) {
        Ok(tokens) => {
            let parser = Parser::new(code, tokens.into_iter());
            parser.parse()
        }
        Err(lex_errors) => Err(lex_errors.into_iter().map(ParseError::from).collect())
    }
}
