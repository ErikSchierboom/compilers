use crate::location::Span;
use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub enum LexError {
    ExpectedCharacter(Span),
    UnexpectedCharacter(char, Span),
    InvalidEscape(char, Span),
}

impl LexError {
    pub fn location(&self) -> &Span {
        match self {
            LexError::ExpectedCharacter(location) |
            LexError::UnexpectedCharacter(_, location) |
            LexError::InvalidEscape(_, location) => location
        }
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    // Literals
    Int(Span),
    Float(Span),
    Char(Span),
    String(Span),
    Quote(Span),
    Word(Span),

    // Delimiters
    OpenBracket(Span),
    CloseBracket(Span),
    OpenParen(Span),
    CloseParen(Span),
}

impl Token {
    pub fn location(&self) -> &Span {
        match self {
            Token::Int(location) |
            Token::Float(location) |
            Token::Char(location) |
            Token::String(location) |
            Token::Quote(location) |
            Token::Word(location) |
            Token::OpenBracket(location) |
            Token::CloseBracket(location) |
            Token::OpenParen(location) |
            Token::CloseParen(location) => location
        }
    }
}

struct Lexer<T: Iterator<Item=char>> {
    chars: Peekable<Enumerate<T>>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.enumerate().peekable() }
    }

    // TODO: lex whitespace
    fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some((start, c, end)) = self.advance() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '[' => tokens.push(Token::OpenBracket(Span { start, end })),
                ']' => tokens.push(Token::CloseBracket(Span { start, end })),
                '(' => tokens.push(Token::OpenParen(Span { start, end })),
                ')' => tokens.push(Token::CloseParen(Span { start, end })),
                '\'' => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Token::Quote(Span { start, end }))
                }
                '0'..='9' => {
                    let end = self.advance_while(char::is_ascii_digit, start);

                    if self.advance_if_eq(&'.') {
                        let end = self.advance_while(char::is_ascii_digit, end + 1);
                        tokens.push(Token::Float(Span { start, end }))
                    } else {
                        tokens.push(Token::Int(Span { start, end }))
                    }
                }
                '#' => {
                    match self.advance() {
                        Some((start, '\\', end)) => {
                            match self.advance() {
                                Some((_, 'n', end)) |
                                Some((_, 'r', end)) |
                                Some((_, 't', end)) |
                                Some((_, '\'', end)) => tokens.push(Token::Char(Span { start, end })),
                                Some((start, c, end)) => errors.push(LexError::InvalidEscape(c, Span { start, end })),
                                None => errors.push(LexError::ExpectedCharacter(Span { start, end })),
                            }
                        }
                        Some((start, _, end)) => tokens.push(Token::Char(Span { start, end })),
                        None => errors.push(LexError::ExpectedCharacter(Span { start: start + 1, end: end + 1 }))
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((start, '"', end)) => tokens.push(Token::String(Span { start, end })),
                            Some((start, '\\', end)) => match self.advance() {
                                Some((_, 'n', _)) |
                                Some((_, 'r', _)) |
                                Some((_, 't', _)) |
                                Some((_, '"', _)) => {}
                                Some((start, c, end)) => errors.push(LexError::InvalidEscape(c, Span { start, end })),
                                None => errors.push(LexError::ExpectedCharacter(Span { start, end })),
                            },
                            Some(_) |
                            None => errors.push(LexError::ExpectedCharacter(Span { start: start + 1, end: end + 1 })),
                        }
                    }
                }
                c if Self::is_word_character(&c) => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Token::Word(Span { start, end }))
                }
                _ => errors.push(LexError::UnexpectedCharacter(c, Span { start, end })),
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn is_word_character(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
    }

    fn advance(&mut self) -> Option<(usize, char, usize)> {
        self.chars.next().map(|(start, c)| (start, c, start + 1))
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool, start: usize) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count() + start + 1
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, Vec<LexError>> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
