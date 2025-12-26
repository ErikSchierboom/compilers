use crate::location::Span;
use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub location: Span,
}

#[derive(Debug)]
pub enum LexErrorKind {
    ExpectedIdentifier,
    UnknownIdentifier(String),
    UnexpectedToken(char),
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
    Word,
    Quote,

    // Binary operators
    Add,
    Mul,

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
    Sub,
    Div,
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
        while let Some((start, char)) = self.chars.next() {
            match char {
                ' ' | '\r' | '\n' | '\t' => continue,
                '+' => self.emit(TokenKind::Add, start, start + 1),
                '-' => self.emit(TokenKind::Sub, start, start + 1),
                '*' => self.emit(TokenKind::Mul, start, start + 1),
                '/' => self.emit(TokenKind::Div, start, start + 1),
                '@' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Read } else { TokenKind::ReadVariable };
                    self.emit(kind, start, start + 1 + length)
                }
                '%' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Write } else { TokenKind::WriteVariable };
                    self.emit(kind, start, start + 1 + length)
                }
                '!' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric);
                    let kind = if length == 0 { TokenKind::Execute } else { TokenKind::ExecuteVariable };
                    self.emit(kind, start, start + 1 + length)
                }
                '[' => self.emit(TokenKind::OpenBracket, start, start + 1),
                ']' => self.emit(TokenKind::CloseBracket, start, start + 1),
                '(' => self.emit(TokenKind::OpenParen, start, start + 1),
                ')' => self.emit(TokenKind::CloseParen, start, start + 1),
                '\'' => self.emit(TokenKind::Quote, start, start + 1),
                '0'..='9' => {
                    let length = self.advance_while(char::is_ascii_digit) + 1;
                    self.emit(TokenKind::Int, start, start + length)
                }
                'a'..='z' | 'A'..='Z' => {
                    let length = self.advance_while(char::is_ascii_alphanumeric) + 1;
                    self.emit(TokenKind::Word, start, start + length)
                }
                _ => return Err(LexError { kind: LexErrorKind::UnexpectedToken(char), location: Span { start, end: start + 1 } })
            }
        }

        Ok(self.tokens)
    }

    fn emit(&mut self, kind: TokenKind, start: usize, end: usize) {
        let token = Token { kind, location: Span { start, end } };
        self.tokens.push(token)
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c)))
            .count()
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
