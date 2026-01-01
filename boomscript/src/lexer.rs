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
    UnexpectedCharacter(char),
    InvalidEscape(char),
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
                '[' => tokens.push(Token { kind: TokenKind::OpenBracket, location: Span { start, end } }),
                ']' => tokens.push(Token { kind: TokenKind::CloseBracket, location: Span { start, end } }),
                '(' => tokens.push(Token { kind: TokenKind::OpenParen, location: Span { start, end } }),
                ')' => tokens.push(Token { kind: TokenKind::CloseParen, location: Span { start, end } }),
                '\'' => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Token { kind: TokenKind::Quote, location: Span { start, end } })
                }
                '0'..='9' => {
                    let end = self.advance_while(char::is_ascii_digit, start);

                    if self.advance_if_eq(&'.') {
                        let end = self.advance_while(char::is_ascii_digit, end + 1);
                        tokens.push(Token { kind: TokenKind::Float, location: Span { start, end } })
                    } else {
                        tokens.push(Token { kind: TokenKind::Int, location: Span { start, end } })
                    }
                }
                '#' => {
                    match self.advance() {
                        Some((start, '\\', end)) => {
                            match self.advance() {
                                Some((_, 'n', end)) |
                                Some((_, 'r', end)) |
                                Some((_, 't', end)) |
                                Some((_, '\'', end)) => tokens.push(Token { kind: TokenKind::Char, location: Span { start, end } }),
                                Some((start, c, end)) => errors.push(LexError { kind: LexErrorKind::InvalidEscape(c), location: Span { start, end } }),
                                None => errors.push(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start, end } }),
                            }
                        }
                        Some((start, _, end)) => tokens.push(Token { kind: TokenKind::Char, location: Span { start, end } }),
                        None => errors.push(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: start + 1, end: end + 1 } })
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((start, '"', end)) => tokens.push(Token { kind: TokenKind::String, location: Span { start, end } }),
                            Some((start, '\\', end)) => match self.advance() {
                                Some((_, 'n', _)) |
                                Some((_, 'r', _)) |
                                Some((_, 't', _)) |
                                Some((_, '"', _)) => {}
                                Some((start, c, end)) => errors.push(LexError { kind: LexErrorKind::InvalidEscape(c), location: Span { start, end } }),
                                None => errors.push(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start, end } }),
                            },
                            Some(_) => {}
                            None => errors.push(LexError { kind: LexErrorKind::ExpectedCharacter, location: Span { start: start + 1, end: end + 1 } }),
                        }
                    }
                }
                c if Self::is_word_character(&c) => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Token { kind: TokenKind::Word, location: Span { start, end } })
                }
                _ => errors.push(LexError { kind: LexErrorKind::UnexpectedCharacter(c), location: Span { start, end } }),
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
