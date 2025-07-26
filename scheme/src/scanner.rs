use std::iter::Peekable;
use crate::scanner::ScanError::UnexpectedCharacter;
use crate::scanner::Token::{Identifier, Number};

#[derive(Debug, Clone)]
pub enum ScanError {
    UnexpectedCharacter(char)
}

#[derive(Debug, Clone)]
pub enum Token {
    LParen,
    RParen,
    Identifier(String),
    Number(i32)
}

type ScanResult<T> = Result<T, ScanError>;

struct Scanner<'a> {
    source_code: &'a str,
    chars: Peekable<std::str::Chars<'a>>,
    start: usize,
    current: usize
}

impl<'a> Scanner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            chars: source_code.chars().peekable(),
            start: 0,
            current: 0
        }
    }

    pub fn scan_token(&mut self) -> Option<ScanResult<Token>> {
        self.skip_whitespace();

        self.start = self.current;

        match self.advance()? {
            '(' => Some(Ok(Token::LParen)),
            ')' => Some(Ok(Token::RParen)),
            c if c.is_ascii_alphabetic() => Some(self.identifier()),
            c if c.is_ascii_digit() => Some(self.number()),
            c => Some(Err(UnexpectedCharacter(c)))
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace)
    }

    fn number(&mut self) -> ScanResult<Token> {
        self.advance_while(char::is_ascii_digit);

        Ok(Number(self.lexeme().parse().unwrap()))
    }

    fn identifier(&mut self) -> ScanResult<Token> {
        self.advance_while(char::is_ascii_alphabetic);

        Ok(Identifier(self.lexeme().to_string()))
    }

    fn lexeme(&mut self) -> &'a str {
        &self.source_code[self.start..self.current]
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.current += 1 )
    }

    fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }
}

pub fn scan(source_code: &str) -> Vec<Token> {
    let mut scanner = Scanner::new(source_code);
    let mut tokens: Vec<Token> = Vec::new();

    while let Some(scan_result) = scanner.scan_token() {
        match scan_result {
            Ok(token) => tokens.push(token),
            Err(error) => eprintln!("{:?}", error)
        }
    }

    tokens
}
