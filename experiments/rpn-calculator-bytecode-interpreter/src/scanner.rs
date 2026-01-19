use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum ScanError {
    UnexpectedChar
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Numeric,
    Plus,
    Minus,
    Star,
    Slash,
    Error(ScanError)
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub stop: usize,
}

pub struct Scanner<'a> {
    chars: Peekable<std::str::Chars<'a>>,
    start: usize,
    current: usize
}

impl<'a> Scanner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            chars: source_code.chars().peekable(),
            start: 0,
            current: 0
        }
    }

    pub fn scan_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.start = self.current;

        match self.advance()? {
            '+' => Some(self.token(TokenKind::Plus)),
            '-' => Some(self.token(TokenKind::Minus)),
            '*' => Some(self.token(TokenKind::Star)),
            '/' => Some(self.token(TokenKind::Slash)),
            c if c.is_ascii_digit() => Some(self.number()),
            _ => Some(self.token(TokenKind::Error(ScanError::UnexpectedChar)))
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(|c| c.is_whitespace())
    }

    fn number(&mut self) -> Token {
        self.advance_while(char::is_ascii_digit);
        self.token(TokenKind::Numeric)
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.current += 1)
    }

    fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }

    fn token(&self, kind: TokenKind) -> Token {
        Token { kind, start: self.start, stop: self.current }
    }
}

