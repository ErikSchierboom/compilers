use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub enum LexErrorKind {
    ExpectedIdentifier,
    UnknownIdentifier(String),
    UnexpectedToken(char),
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub location: Span,
}

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const EMPTY: Self = Self { start: 0, end: 0 };

    pub fn merge(&self, other: &Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::EMPTY
    }
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
                '+' => self.emit_token(TokenKind::Add, start_pos, start_pos + 1),
                '*' => self.emit_token(TokenKind::Mul, start_pos, start_pos + 1),
                '@' => self.emit_token(TokenKind::Read, start_pos, start_pos + 1),
                '%' => self.emit_token(TokenKind::Write, start_pos, start_pos + 1),
                '!' => self.emit_token(TokenKind::Execute, start_pos, start_pos + 1),
                '[' => self.emit_token(TokenKind::OpenBracket, start_pos, start_pos + 1),
                ']' => self.emit_token(TokenKind::CloseBracket, start_pos, start_pos + 1),
                '(' => self.emit_token(TokenKind::OpenParen, start_pos, start_pos + 1),
                ')' => self.emit_token(TokenKind::CloseParen, start_pos, start_pos + 1),
                '\'' => self.emit_token(TokenKind::Quote, start_pos, start_pos + 1),
                '0'..='9' => {
                    let mut end_pos = start_pos + 1;

                    while let Some((next_pos, _)) = self.chars.next_if(|(_, c)| c.is_ascii_digit()) {
                        end_pos = next_pos + 1
                    }

                    self.emit_token(TokenKind::Int, start_pos, end_pos)
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut end_pos = start_pos + 1;

                    while let Some((next_pos, _)) = self.chars.next_if(|(_, c)| c.is_ascii_alphanumeric()) {
                        end_pos = next_pos + 1
                    }

                    self.emit_token(TokenKind::Identifier, start_pos, end_pos)
                }
                _ => return Err(LexError { kind: LexErrorKind::UnexpectedToken(char), location: Span { start: start_pos, end: start_pos + 1 } })
            }
        }

        Ok(self.tokens)
    }

    fn emit_token(&mut self, token_kind: TokenKind, start: usize, end: usize) {
        let token = Token { kind: token_kind, location: Span { start, end } };
        self.tokens.push(token)
    }
}

// TODO: add iterator implementation

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
