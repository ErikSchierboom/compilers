use crate::lexer::{tokenize, LexError, Token};
use crate::location::Span;
use crate::parser::ParseError::Lex;
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError, Span),
    UnexpectedToken(String, Span),
    UnexpectedEndOfFile(Span),
}

impl ParseError {
    pub fn location(&self) -> &Span {
        match self {
            Lex(_, location) |
            ParseError::UnexpectedToken(_, location) |
            ParseError::UnexpectedEndOfFile(location) => location
        }
    }
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        let location = value.location().clone();
        Lex(value, location)
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int(i64, Span),
    Float(f64, Span),
    Char(char, Span),
    String(String, Span),
    Quote(String, Span),
    Name(String, Span),

    // Composite
    Block(Vec<Word>, Span),
    Array(Vec<Word>, Span),
}

impl Word {
    pub fn location(&self) -> &Span {
        match self {
            Word::Int(_, location) |
            Word::Float(_, location) |
            Word::Char(_, location) |
            Word::String(_, location) |
            Word::Quote(_, location) |
            Word::Name(_, location) |
            Word::Block(_, location) |
            Word::Array(_, location) => location,
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
                Err(parse_error) => errors.extend(parse_error)
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn parse_word(&mut self) -> Option<Result<Word, Vec<ParseError>>> {
        let token = self.tokens.next()?;

        match token {
            Token::Int(location) => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Word::Int(value, location)))
            }
            Token::Float(location) => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Word::Float(value, location)))
            }
            Token::Char(location) => {
                let value = match &self.lexeme(&location)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                Some(Ok(Word::Char(value, location)))
            }
            Token::String(location) => {
                let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Some(Ok(Word::String(value, location)))
            }
            Token::Quote(location) => {
                let name = self.lexeme(&location)[1..].into();
                Some(Ok(Word::Quote(name, location)))
            }
            Token::Word(location) => {
                let name = self.lexeme(&location).into();
                Some(Ok(Word::Name(name, location)))
            }
            Token::OpenBracket(location) => {
                match self.parse_delimited(|token| matches!(token, Token::CloseBracket(_)), location) {
                    Ok((words, location)) => Some(Ok(Word::Array(words, location))),
                    Err(err) => Some(Err(err))
                }
            }
            Token::OpenParen(location) => {
                match self.parse_delimited(|token| matches!(token, Token::CloseParen(_)), location) {
                    Ok((words, location)) => Some(Ok(Word::Block(words, location))),
                    Err(err) => Some(Err(err))
                }
            }
            Token::CloseBracket(location) |
            Token::CloseParen(location) => Some(Err(vec![ParseError::UnexpectedToken(self.lexeme(&location).into(), location.clone())])),
        }
    }

    fn parse_delimited(&mut self, stop_parsing: impl Fn(&Token) -> bool, start: Span) -> Result<(Vec<Word>, Span), Vec<ParseError>> {
        let mut words = Vec::new();
        let mut errors = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| stop_parsing(token)) {
                let location = start.merge(&token.location());
                if errors.is_empty() {
                    return Ok((words, location));
                } else {
                    return Err(errors);
                }
            }

            match self.parse_word() {
                None => {
                    errors.push(ParseError::UnexpectedEndOfFile(Span { start: self.code.len(), end: self.code.len() + 1 }));
                    return Err(errors);
                }
                Some(Ok(word)) => words.push(word),
                Some(Err(err)) => errors.extend(err)
            }
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
        Err(errors) => Err(errors.into_iter().map(ParseError::from).collect())
    }
}
