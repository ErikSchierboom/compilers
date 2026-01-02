use crate::lexer::{tokenize, LexError, Token};
use crate::location::{Span, Spanned};
use crate::parser::ParseError::Lex;
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    UnexpectedEndOfFile,
}


impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        Lex(value)
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    QuotedWord(String),
    Word(String),

    // Composite
    Block(Vec<Spanned<Word>>),
    Array(Vec<Spanned<Word>>),
}

struct Parser<'a, T: Iterator<Item=Spanned<Token>>> {
    code: &'a str,
    tokens: Peekable<T>,
}

impl<'a, T: Iterator<Item=Spanned<Token>>> Parser<'a, T> {
    fn new(code: &'a str, tokens: T) -> Self {
        Self { code, tokens: tokens.peekable() }
    }

    fn parse(mut self) -> Result<Vec<Spanned<Word>>, Vec<Spanned<ParseError>>> {
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

    fn parse_word(&mut self) -> Option<Result<Spanned<Word>, Vec<Spanned<ParseError>>>> {
        let spanned = self.tokens.next()?;
        let Spanned { value: token, span: location } = spanned;

        match token {
            Token::Int => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Spanned::new(Word::Int(value), location)))
            }
            Token::Float => {
                let value = self.lexeme(&location).parse().unwrap();
                Some(Ok(Spanned::new(Word::Float(value), location)))
            }
            Token::Char => {
                let value = match &self.lexeme(&location)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                Some(Ok(Spanned::new(Word::Char(value), location)))
            }
            Token::String => {
                let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Some(Ok(Spanned::new(Word::String(value), location)))
            }
            Token::Quote => {
                let name = self.lexeme(&location)[1..].into();
                Some(Ok(Spanned::new(Word::QuotedWord(name), location)))
            }
            Token::Word => {
                let name = self.lexeme(&location).into();
                Some(Ok(Spanned::new(Word::Word(name), location)))
            }
            Token::OpenBracket => {
                match self.parse_delimited(Token::CloseBracket, location) {
                    Ok((words, location)) => Some(Ok(Spanned::new(Word::Array(words), location))),
                    Err(err) => Some(Err(err))
                }
            }
            Token::OpenParen => {
                match self.parse_delimited(Token::CloseParen, location) {
                    Ok((words, location)) => Some(Ok(Spanned::new(Word::Block(words), location))),
                    Err(err) => Some(Err(err))
                }
            }
            Token::CloseBracket |
            Token::CloseParen => Some(Err(vec![Spanned::new(ParseError::UnexpectedToken(token), location)])),
        }
    }

    fn parse_delimited(&mut self, close_delimiter: Token, start: Span) -> Result<(Vec<Spanned<Word>>, Span), Vec<Spanned<ParseError>>> {
        let mut words = Vec::new();
        let mut errors = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.value == close_delimiter) {
                let location = start.merge(&token.span);
                if errors.is_empty() {
                    return Ok((words, location));
                } else {
                    return Err(errors);
                }
            }

            match self.parse_word() {
                None => {
                    errors.push(Spanned::new(ParseError::UnexpectedEndOfFile, Span { start: self.code.len(), end: self.code.len() + 1 }));
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

pub fn parse(code: &str) -> Result<Vec<Spanned<Word>>, Vec<Spanned<ParseError>>> {
    match tokenize(code) {
        Ok(tokens) => {
            let parser = Parser::new(code, tokens.into_iter());
            parser.parse()
        }
        Err(errors) => Err(errors.into_iter().map(|error| error.map(ParseError::from)).collect())
    }
}
