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
    Char,
    String,
    Word,
    Quote,

    // Math operators
    Plus,
    Minus,
    Star,
    Slash,

    // Binary operators
    Ampersand,
    Pipe,
    Bang,
    Caret,

    // Comparison operators
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    BangEqual,

    // Memory operators
    Read,
    ReadVariable,
    Write,
    WriteVariable,
    Execute,
    ExecuteVariable,

    // Delimiters
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    PlusPlus,
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
        while let Some((start_pos, char)) = self.advance() {
            match char {
                ' ' | '\r' | '\n' | '\t' => continue,
                '+' => {
                    if self.advance_if_eq(&'+') {
                        self.emit(TokenKind::PlusPlus, start_pos, start_pos + 1)
                    } else {
                        self.emit(TokenKind::Plus, start_pos, start_pos + 1)
                    }
                }
                '-' => self.emit(TokenKind::Minus, start_pos, start_pos + 1),
                '*' => self.emit(TokenKind::Star, start_pos, start_pos + 1),
                '/' => self.emit(TokenKind::Slash, start_pos, start_pos + 1),
                '&' => self.emit(TokenKind::Ampersand, start_pos, start_pos + 1),
                '|' => self.emit(TokenKind::Pipe, start_pos, start_pos + 1),
                '^' => self.emit(TokenKind::Caret, start_pos, start_pos + 1),
                '=' => self.emit(TokenKind::Equal, start_pos, start_pos + 1),
                '!' => {
                    if self.advance_if_eq(&'=') {
                        self.emit(TokenKind::BangEqual, start_pos, start_pos + 2)
                    } else {
                        self.emit(TokenKind::Bang, start_pos, start_pos + 1)
                    }
                }
                '<' => {
                    if self.advance_if_eq(&'=') {
                        self.emit(TokenKind::LessEqual, start_pos, start_pos + 2)
                    } else {
                        self.emit(TokenKind::Less, start_pos, start_pos + 1)
                    }
                }
                '>' => {
                    if self.advance_if_eq(&'=') {
                        self.emit(TokenKind::GreaterEqual, start_pos, start_pos + 2)
                    } else {
                        self.emit(TokenKind::Greater, start_pos, start_pos + 1)
                    }
                }
                '@' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Read } else { TokenKind::ReadVariable };
                    self.emit(kind, start_pos, start_pos + 1 + length)
                }
                '$' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Write } else { TokenKind::WriteVariable };
                    self.emit(kind, start_pos, start_pos + 1 + length)
                }
                '%' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Execute } else { TokenKind::ExecuteVariable };
                    self.emit(kind, start_pos, start_pos + 1 + length)
                }
                '[' => self.emit(TokenKind::OpenBracket, start_pos, start_pos + 1),
                ']' => self.emit(TokenKind::CloseBracket, start_pos, start_pos + 1),
                '(' => self.emit(TokenKind::OpenParen, start_pos, start_pos + 1),
                ')' => self.emit(TokenKind::CloseParen, start_pos, start_pos + 1),
                '\'' => self.emit(TokenKind::Quote, start_pos, start_pos + 1),
                '0'..='9' => {
                    let length = self.advance_while(char::is_ascii_digit) + 1;
                    self.emit(TokenKind::Int, start_pos, start_pos + length)
                }
                'a'..='z' | 'A'..='Z' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric) + 1;
                    self.emit(TokenKind::Word, start_pos, start_pos + length)
                }
                '#' => {
                    match self.advance() {
                        Some((backslash_pos, '\\')) => {
                            match self.advance() {
                                Some((end_pos, 'n')) |
                                Some((end_pos, 'r')) |
                                Some((end_pos, 't')) |
                                Some((end_pos, '\'')) => self.emit(TokenKind::Char, start_pos, end_pos + 1),
                                Some((escape_pos, _)) => Err(Self::error(LexErrorKind::InvalidEscape, escape_pos, escape_pos + 1))?,
                                None => return Err(Self::error(LexErrorKind::ExpectedCharacter, backslash_pos, backslash_pos + 1))
                            }
                        }
                        Some((end_pos, _)) => self.emit(TokenKind::Char, start_pos, end_pos + 1),
                        None => return Err(Self::error(LexErrorKind::ExpectedCharacter, start_pos + 1, start_pos + 2))
                    };
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((end_pos, '"')) => {
                                self.emit(TokenKind::String, start_pos, end_pos + 1);
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
                            None => return Err(Self::error(LexErrorKind::ExpectedCharacter, start_pos + 1, start_pos + 2))
                        };
                    }
                }
                _ => return Err(Self::error(LexErrorKind::UnexpectedToken(char), start_pos, start_pos + 1))
            }
        }

        Ok(self.tokens)
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
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
