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
    Identifier,
    Quote,

    // Binary operators
    Add,
    Mul,

    // Memory operators
    Read,
    Write,
    Execute,

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
        while let Some((start_pos, char)) = self.chars.next() {
            match char {
                ' ' | '\r' | '\n' | '\t' => {
                    continue
                }
                '+' => self.emit(TokenKind::Add, start_pos, start_pos + 1),
                '*' => self.emit(TokenKind::Mul, start_pos, start_pos + 1),
                '@' => self.emit(TokenKind::Read, start_pos, start_pos + 1),
                '%' => self.emit(TokenKind::Write, start_pos, start_pos + 1),
                '!' => self.emit(TokenKind::Execute, start_pos, start_pos + 1),
                '[' => self.emit(TokenKind::OpenBracket, start_pos, start_pos + 1),
                ']' => self.emit(TokenKind::CloseBracket, start_pos, start_pos + 1),
                '(' => self.emit(TokenKind::OpenParen, start_pos, start_pos + 1),
                ')' => self.emit(TokenKind::CloseParen, start_pos, start_pos + 1),
                '\'' => self.emit(TokenKind::Quote, start_pos, start_pos + 1),
                '0'..='9' => {
                    let mut end_pos = start_pos + 1;

                    while self.chars.next_if(|(_, c)| c.is_ascii_digit()).is_some() {
                        end_pos += 1
                    }

                    self.emit(TokenKind::Int, start_pos, end_pos)
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut end_pos = start_pos + 1;

                    while self.chars.next_if(|(_, c)| c.is_ascii_alphanumeric()).is_some() {
                        end_pos += 1
                    }

                    self.emit(TokenKind::Identifier, start_pos, end_pos)
                }
                _ => return Err(LexError { kind: LexErrorKind::UnexpectedToken(char), location: Span { start: start_pos, end: start_pos + 1 } })
            }
        }

        Ok(self.tokens)
    }

    fn emit(&mut self, kind: TokenKind, start: usize, end: usize) {
        let token = Token { kind, location: Span { start, end } };
        self.tokens.push(token)
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
