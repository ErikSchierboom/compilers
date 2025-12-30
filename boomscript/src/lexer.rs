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
    // TODO: support floats
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

    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while let Some((start, c)) = self.advance() {
            match c {
                ' ' | '\r' | '\n' | '\t' => continue,
                '[' => self.emit(TokenKind::OpenBracket, start, start + 1),
                ']' => self.emit(TokenKind::CloseBracket, start, start + 1),
                '(' => self.emit(TokenKind::OpenParen, start, start + 1),
                ')' => self.emit(TokenKind::CloseParen, start, start + 1),
                '\'' => {
                    let length = self.advance_while(Self::is_word_character) + 1;
                    self.emit(TokenKind::Quote, start, start + length)
                }
                '0'..='9' => {
                    let length = self.advance_while(char::is_ascii_digit) + 1;
                    self.emit(TokenKind::Int, start, start + length)
                }
                '#' => {
                    match self.advance() {
                        Some((backslash_pos, '\\')) => {
                            match self.advance() {
                                Some((end_pos, 'n')) |
                                Some((end_pos, 'r')) |
                                Some((end_pos, 't')) |
                                Some((end_pos, '\'')) => self.emit(TokenKind::Char, start, end_pos + 1),
                                Some((escape_pos, _)) => Err(Self::error(LexErrorKind::InvalidEscape, escape_pos, escape_pos + 1))?,
                                None => return Err(Self::error(LexErrorKind::ExpectedCharacter, backslash_pos, backslash_pos + 1))
                            }
                        }
                        Some((end_pos, _)) => self.emit(TokenKind::Char, start, end_pos + 1),
                        None => return Err(Self::error(LexErrorKind::ExpectedCharacter, start + 1, start + 2))
                    };
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((end_pos, '"')) => {
                                self.emit(TokenKind::String, start, end_pos + 1);
                                break;
                            }
                            Some((backslash_pos, '\\')) => {
                                match self.advance() {
                                    Some((_, 'n')) |
                                    Some((_, 'r')) |
                                    Some((_, 't')) |
                                    Some((_, '"')) => {}
                                    Some((escape_pos, _)) => Err(Self::error(LexErrorKind::InvalidEscape, escape_pos, escape_pos + 1))?,
                                    None => return Err(Self::error(LexErrorKind::ExpectedCharacter, backslash_pos, backslash_pos + 1))
                                }
                            }
                            Some(_) => {}
                            None => return Err(Self::error(LexErrorKind::ExpectedCharacter, start + 1, start + 2))
                        };
                    }
                }
                c if Self::is_word_character(&c) => {
                    let length = self.advance_while(Self::is_word_character) + 1;
                    self.emit(TokenKind::Word, start, start + length)
                }
                _ => return Err(Self::error(LexErrorKind::UnexpectedToken(c), start, start + 1))
            }
        }

        Ok(self.tokens)
    }

    fn is_word_character(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c)))
            .count()
    }

    fn emit(&mut self, kind: TokenKind, start: usize, end: usize) {
        let token = Token { kind, location: Span { start, end } };
        self.tokens.push(token)
    }

    fn error(kind: LexErrorKind, start: usize, end: usize) -> LexError {
        LexError { kind, location: Span { start, end } }
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
