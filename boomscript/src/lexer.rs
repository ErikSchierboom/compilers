use crate::location::Span;
use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub location: Span,
}

#[derive(Debug)]
pub enum LexErrorKind {
    ExpectedCharacter,
    UnexpectedToken(char),
    InvalidEscape,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub location: Span,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Literals
    Int,
    Float,
    Char,
    String,
    Quote,
    Word,

    // Delimiters
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
}

struct Lexer<T: Iterator<Item=char>> {
    chars: Peekable<Enumerate<T>>
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.enumerate().peekable() }
    }

    // TODO: return multiple errors
    // TODO: lex whitespace
    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        std::iter::from_fn(|| self.lex_token()).collect()
    }

    fn lex_token(&mut self) -> Option<Result<Token, LexError>> {
        self.advance_while(char::is_ascii_whitespace);

        // TODO: maybe also return end?
        let (start, c) = self.advance()?;
        
        match c {
            '[' => Some(Ok(Token { kind: TokenKind::OpenBracket, location: Span { start, end: start + 1 } })),
            ']' => Some(Ok(Token { kind: TokenKind::CloseBracket, location: Span { start, end: start + 1 } })),
            '(' => Some(Ok(Token { kind: TokenKind::OpenParen, location: Span { start, end: start + 1 } })),
            ')' => Some(Ok(Token { kind: TokenKind::CloseParen, location: Span { start, end: start + 1 } })),
            '\'' => {
                let length = self.advance_while(Self::is_word_character) + 1;
                Some(Ok(Token { kind: TokenKind::Quote, location: Span { start, end: start + length } }))
            }
            '0'..='9' => {
                let length = self.advance_while(char::is_ascii_digit) + 1;

                if self.advance_if_eq(&'.') {
                    let precision_length = self.advance_while(char::is_ascii_digit) + 1;
                    Some(Ok(Token { kind: TokenKind::Float, location: Span { start, end: start + length + precision_length } }))
                } else {
                    Some(Ok(Token { kind: TokenKind::Int, location: Span { start, end: start + length } }))
                }
            }
            '#' => {
                match self.advance() {
                    Some((backslash_pos, '\\')) => {
                        match self.advance() {
                            Some((end_pos, 'n')) |
                            Some((end_pos, 'r')) |
                            Some((end_pos, 't')) |
                            Some((end_pos, '\'')) => {
                                Some(Ok(Token { kind: TokenKind::Char, location: Span { start, end: end_pos + 1 } }))
                            }
                            Some((escape_pos, _)) => {
                                Some(Err(LexError { kind: LexErrorKind::InvalidEscape, location: Span { start: escape_pos, end: escape_pos + 1 } }))
                            }
                            None => {
                                Some(Err(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: backslash_pos, end: backslash_pos + 1 } }))
                            }
                        }
                    }
                    Some((end_pos, _)) => Some(Ok(Token { kind: TokenKind::Char, location: Span { start, end: end_pos + 1 } })),
                    None => Some(Err(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: start + 1, end: start + 2 } })))
                }
            }
            '"' => {
                loop {
                    match self.advance() {
                        Some((end_pos, '"')) => {
                            return Some(Ok(Token { kind: TokenKind::String, location: Span { start, end: end_pos + 1 } }))
                        }
                        Some((backslash_pos, '\\')) => match self.advance() {
                            Some((_, 'n')) |
                            Some((_, 'r')) |
                            Some((_, 't')) |
                            Some((_, '"')) => {}
                            Some((escape_pos, _)) => return Some(Err(LexError { kind: LexErrorKind::InvalidEscape, location: Span { start: escape_pos, end: escape_pos + 1 } })),
                            None => return Some(Err(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: backslash_pos, end: backslash_pos + 1 } })),
                        },
                        Some(_) => {}
                        None => {
                            return Some(Err(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: start + 1, end: start + 2 } }))
                        }
                    }
                }
            }
            c if Self::is_word_character(&c) => {
                let length = self.advance_while(Self::is_word_character) + 1;
                Some(Ok(Token { kind: TokenKind::Word, location: Span { start, end: start + length } }))
            }
            _ => Some(Err(LexError { kind: LexErrorKind::UnexpectedToken(c), location: Span { start, end: start + 1 } })),
        }
    }

    fn is_word_character(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count()
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
