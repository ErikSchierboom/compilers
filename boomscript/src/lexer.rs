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
    chars: Peekable<Enumerate<T>>,
    tokens: Vec<Token>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.enumerate().peekable(), tokens: Vec::new() }
    }

    // TODO: return multiple errors
    // TODO: lex whitespace
    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        std::iter::from_fn(|| self.lex_token()).collect()
    }

    fn lex_token(&mut self) -> Option<Result<Token, LexError>> {
        self.advance_while(char::is_ascii_whitespace);

        let (start, c) = self.advance()?;
        let result = match c {
            '[' => Ok(Self::token(TokenKind::OpenBracket, start, start + 1)),
            ']' => Ok(Self::token(TokenKind::CloseBracket, start, start + 1)),
            '(' => Ok(Self::token(TokenKind::OpenParen, start, start + 1)),
            ')' => Ok(Self::token(TokenKind::CloseParen, start, start + 1)),
            '\'' => {
                let length = self.advance_while(Self::is_word_character) + 1;
                Ok(Self::token(TokenKind::Quote, start, start + length))
            }
            '0'..='9' => {
                let length = self.advance_while(char::is_ascii_digit) + 1;

                if self.advance_if_eq(&'.') {
                    let precision_length = self.advance_while(char::is_ascii_digit) + 1;
                    Ok(Self::token(TokenKind::Float, start, start + length + precision_length))
                } else {
                    Ok(Self::token(TokenKind::Int, start, start + length))
                }
            }
            '#' => {
                match self.advance() {
                    Some((backslash_pos, '\\')) => {
                        match self.advance() {
                            Some((end_pos, 'n')) |
                            Some((end_pos, 'r')) |
                            Some((end_pos, 't')) |
                            Some((end_pos, '\'')) => Ok(Self::token(TokenKind::Char, start, end_pos + 1)),
                            Some((escape_pos, _)) => Err(Self::error(LexErrorKind::InvalidEscape, escape_pos, escape_pos + 1)),
                            None => Err(Self::error(LexErrorKind::ExpectedCharacter, backslash_pos, backslash_pos + 1))
                        }
                    }
                    Some((end_pos, _)) => Ok(Self::token(TokenKind::Char, start, end_pos + 1)),
                    None => Err(Self::error(LexErrorKind::ExpectedCharacter, start + 1, start + 2))
                }
            }
            '"' => {
                loop {
                    match self.advance() {
                        Some((end_pos, '"')) => {
                            return Some(Ok(Self::token(TokenKind::String, start, end_pos + 1)))
                        }
                        Some((backslash_pos, '\\')) => {
                            match self.advance() {
                                Some((_, 'n')) |
                                Some((_, 'r')) |
                                Some((_, 't')) |
                                Some((_, '"')) => {}
                                Some((escape_pos, _)) => return Some(Err(Self::error(LexErrorKind::InvalidEscape, escape_pos, escape_pos + 1))),
                                None => return Some(Err(Self::error(LexErrorKind::ExpectedCharacter, backslash_pos, backslash_pos + 1)))
                            }
                        }
                        Some(_) => {}
                        None => return Some(Err(Self::error(LexErrorKind::ExpectedCharacter, start + 1, start + 2)))
                    }
                }
            }
            c if Self::is_word_character(&c) => {
                let length = self.advance_while(Self::is_word_character) + 1;
                Ok(Self::token(TokenKind::Word, start, start + length))
            }
            _ => Err(Self::error(LexErrorKind::UnexpectedToken(c), start, start + 1))
        };

        Some(result)
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

    fn token(kind: TokenKind, start: usize, end: usize) -> Token {
        Token { kind, location: Span { start, end } }
    }

    fn error(kind: LexErrorKind, start: usize, end: usize) -> LexError {
        LexError { kind, location: Span { start, end } }
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
