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
    UnexpectedToken(TokenKind),
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
    Char { value: char, location: Span },
    String { value: String, location: Span },
    Quote { name: String, location: Span },
    Identifier { name: String, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { words: Vec<Word>, location: Span },

    // Math operators
    Add { location: Span },
    Sub { location: Span },
    Mul { location: Span },
    Div { location: Span },

    // Bitwise operators
    And { location: Span },
    Or { location: Span },
    Xor { location: Span },
    Not { location: Span },

    // Comparison operators
    Greater { location: Span },
    GreaterEqual { location: Span },
    Less { location: Span },
    LessEqual { location: Span },
    Equal { location: Span },
    NotEqual { location: Span },

    // Memory operators
    Read { location: Span },
    Write { location: Span },
    Execute { location: Span },
}

impl Word {
    fn location(&self) -> &Span {
        match self {
            Word::Int { location, .. } |
            Word::Char { location, .. } |
            Word::String { location, .. } |
            Word::Quote { location, .. } |
            Word::Identifier { location, .. } |
            Word::Block { location, .. } |
            Word::Array { location, .. } |
            Word::Add { location, .. } |
            Word::Sub { location, .. } |
            Word::Mul { location, .. } |
            Word::Div { location, .. } |
            Word::And { location, .. } |
            Word::Or { location, .. } |
            Word::Xor { location, .. } |
            Word::Not { location, .. } |
            Word::Greater { location, .. } |
            Word::GreaterEqual { location, .. } |
            Word::Less { location, .. } |
            Word::LessEqual { location, .. } |
            Word::Equal { location, .. } |
            Word::NotEqual { location, .. } |
            Word::Read { location, .. } |
            Word::Write { location, .. } |
            Word::Execute { location, .. } => location,
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
            TokenKind::Int => {
                let value = self.lexeme(&location).parse().unwrap();
                self.emit(Word::Int { value, location })
            }
            TokenKind::Char => {
                let value = match &self.lexeme(&location)[1..] {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\'" => '\'',
                    lexeme => lexeme.chars().next().unwrap()
                };
                self.emit(Word::Char { value, location })
            }
            TokenKind::String => {
                let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                self.emit(Word::String { value, location })
            }
            TokenKind::Quote => {
                match self.tokens.next() {
                    Some(Token { kind: TokenKind::Identifier, location }) => {
                        let name = self.lexeme(&location).into();
                        self.emit(Word::Quote { name, location })
                    }
                    Some(token) => return Err(Self::error(ParseErrorKind::ExpectedIdentifier, token.location)),
                    None => return Err(Self::error(ParseErrorKind::ExpectedIdentifier, location)),
                }
            }
            TokenKind::Identifier => {
                let name = self.lexeme(&location).into();
                self.emit(Word::Identifier { name, location })
            }

            // TODO: map operators to words (e.g. "+" => "plus")
            TokenKind::Plus => self.emit(Word::Add { location }),
            TokenKind::Minus => self.emit(Word::Sub { location }),
            TokenKind::Star => self.emit(Word::Mul { location }),
            TokenKind::Slash => self.emit(Word::Div { location }),
            TokenKind::PlusPlus => self.emit(Word::Identifier { name: "concat".into(), location }),
            TokenKind::Ampersand => self.emit(Word::And { location }),
            TokenKind::Pipe => self.emit(Word::Or { location }),
            TokenKind::Caret => self.emit(Word::Xor { location }),
            TokenKind::Bang => self.emit(Word::Not { location }),
            TokenKind::Greater => self.emit(Word::Greater { location }),
            TokenKind::GreaterEqual => self.emit(Word::GreaterEqual { location }),
            TokenKind::Less => self.emit(Word::Less { location }),
            TokenKind::LessEqual => self.emit(Word::LessEqual { location }),
            TokenKind::Equal => self.emit(Word::Equal { location }),
            TokenKind::BangEqual => self.emit(Word::NotEqual { location }),
            TokenKind::At => self.emit(Word::Read { location }),
            TokenKind::Dollar => self.emit(Word::Write { location }),
            TokenKind::Percent => self.emit(Word::Execute { location }),
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
