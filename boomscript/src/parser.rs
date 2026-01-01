use crate::lexer::{tokenize, LexError, Token, TokenKind};
use crate::location::Span;
use crate::parser::ParseErrorKind::Lex;
use std::iter::Peekable;

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        let location = value.span.clone();
        Self { kind: Lex(value), span: location }
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, span: Span },
    Float { value: f64, span: Span },
    Char { value: char, span: Span },
    String { value: String, span: Span },
    Quote { name: String, span: Span },
    Name { name: String, span: Span },

    // Composite
    Block { words: Vec<Word>, span: Span },
    Array { words: Vec<Word>, span: Span },
}

impl Word {
    pub fn span(&self) -> &Span {
        match self {
            Word::Int { span, .. } |
            Word::Float { span, .. } |
            Word::Char { span, .. } |
            Word::String { span, .. } |
            Word::Quote { span, .. } |
            Word::Name { span, .. } |
            Word::Block { span, .. } |
            Word::Array { span, .. } => span,
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
        let Token { kind, span } = self.tokens.next()?;

        match kind {
            TokenKind::Int => {
                let value = self.lexeme(&span).parse().unwrap();
                Some(Ok(Word::Int { value, span }))
            }
            TokenKind::Float => {
                let value = self.lexeme(&span).parse().unwrap();
                Some(Ok(Word::Float { value, span }))
            }
            TokenKind::Char => {
                let value = match &self.lexeme(&span)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                Some(Ok(Word::Char { value, span }))
            }
            TokenKind::String => {
                let value = String::from(&self.lexeme(&span)[1..span.end - span.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Some(Ok(Word::String { value, span }))
            }
            TokenKind::Quote => {
                let name = self.lexeme(&span)[1..].into();
                Some(Ok(Word::Quote { name, span }))
            }
            TokenKind::Word => {
                let name = self.lexeme(&span).into();
                Some(Ok(Word::Name { name, span }))
            }
            TokenKind::OpenBracket => {
                match self.parse_delimited(TokenKind::CloseBracket, span) {
                    Ok((words, span)) => Some(Ok(Word::Array { words, span })),
                    Err(err) => Some(Err(err))
                }
            }
            TokenKind::OpenParen => {
                match self.parse_delimited(TokenKind::CloseParen, span) {
                    Ok((words, span)) => Some(Ok(Word::Block { words, span })),
                    Err(err) => Some(Err(err))
                }
            }
            TokenKind::CloseBracket |
            TokenKind::CloseParen => Some(Err(vec![ParseError { kind: ParseErrorKind::UnexpectedToken(kind), span }])),
        }
    }

    fn parse_delimited(&mut self, end_delimiter: TokenKind, start: Span) -> Result<(Vec<Word>, Span), Vec<ParseError>> {
        let mut words = Vec::new();
        let mut errors = Vec::new();

        loop {
            if let Some(token) = self.tokens.next_if(|token| token.kind == end_delimiter) {
                let span = start.merge(&token.span);
                if errors.is_empty() {
                    return Ok((words, span));
                } else {
                    return Err(errors);
                }
            }

            match self.parse_word() {
                None => {
                    errors.push(ParseError { kind: ParseErrorKind::ExpectedToken(end_delimiter), span: Span::EMPTY });
                    return Err(errors);
                }
                Some(Ok(word)) => words.push(word),
                Some(Err(err)) => errors.extend(err)
            }
        }
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.code[span.start..span.end]
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
