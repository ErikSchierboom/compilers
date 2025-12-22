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

impl From<usize> for Span {
    fn from(start: usize) -> Self {
        Self { start, end: start + 1 }
    }
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
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
    char: Option<char>,
    pos: usize,
    tokens: Vec<Token>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        let mut lexer = Self { chars: code.enumerate().peekable(), char: None, pos: 0, tokens: Vec::new() };
        lexer.next_char();
        lexer
    }

    fn next_char_if(&mut self, f: impl FnOnce(&char) -> bool) -> Option<char> {
        match self.chars.next_if(|(_, c)| f(c)) {
            Some((pos, c)) => {
                self.pos = pos;
                self.char = Some(c);
                self.char
            }
            None => None
        }
    }

    fn next_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some((pos, c)) => {
                self.pos = pos;
                self.char = Some(c);
                self.char
            }
            None => {
                self.pos += 1;
                self.char = None;
                self.char
            }
        }
    }

    fn emit(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn eat_single_char(&mut self, token_kind: TokenKind) {
        self.next_char();
        self.emit(Token { kind: token_kind, location: (self.pos - 1, self.pos).into() })
    }

    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while let Some(char) = self.char {
            let start_pos = self.pos;

            match char {
                ' ' | '\r' | '\n' | '\t' => {
                    self.next_char();
                }
                '+' => self.eat_single_char(TokenKind::Add),
                '*' => self.eat_single_char(TokenKind::Mul),
                '@' => self.eat_single_char(TokenKind::Read),
                '%' => self.eat_single_char(TokenKind::Write),
                '!' => self.eat_single_char(TokenKind::Execute),
                '[' => self.eat_single_char(TokenKind::OpenBracket),
                ']' => self.eat_single_char(TokenKind::CloseBracket),
                '(' => self.eat_single_char(TokenKind::OpenParen),
                ')' => self.eat_single_char(TokenKind::CloseParen),
                '\'' => self.eat_single_char(TokenKind::Quote),
                '0'..='9' => {
                    while self.next_char_if(char::is_ascii_digit).is_some() {}
                    self.eat_single_char(TokenKind::Int)
                }
                'a'..='z' | 'A'..='Z' => {
                    while self.next_char_if(char::is_ascii_alphanumeric).is_some() {}
                    self.eat_single_char(TokenKind::Identifier)
                }
                _ => return Err(LexError { kind: LexErrorKind::UnexpectedToken(char), location: (start_pos, start_pos + 1).into() })
            }
        }

        Ok(self.tokens)
    }
}

// TODO: add iterator implementation

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
