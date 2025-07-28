use std::iter::Peekable;
use crate::scanner::SyntaxError::UnexpectedCharacter;
use crate::scanner::Token::{Identifier, Numeric};

#[derive(Debug, Clone)]
pub enum SyntaxError {
    UnexpectedCharacter(char),
    ExpectedCharacter(char)
}

#[derive(Debug, Clone)]
pub enum Token {
    LParen,
    RParen,
    Identifier(String),
    Numeric(String)
}

struct Scanner<'a> {
    source_code: &'a str,
    chars: Peekable<std::str::Chars<'a>>,
    start: usize,
    current: usize
}

impl<'a> Scanner<'a> {
    fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            chars: source_code.chars().peekable(),
            start: 0,
            current: 0
        }
    }
    
    pub fn scan(&mut self) -> (Vec<Token>, Vec<SyntaxError>) {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors: Vec<SyntaxError> = Vec::new();

        while let Some(token_result) = self.scan_token() {
            match token_result {
                Ok(token) => tokens.push(token),
                Err(scan_error) => errors.push(scan_error)
            }

        }

        (tokens, errors)
    }

    pub fn scan_token(&mut self) -> Option<Result<Token, SyntaxError>> {
        self.skip_whitespace();

        self.start = self.current;

        match self.advance()? {
            '(' => Some(Ok(Token::LParen)),
            ')' => Some(Ok(Token::RParen)),
            '+' => Some(Ok(Identifier("+".to_string()))),
            '-' => Some(Ok(Identifier("-".to_string()))),
            '*' => Some(Ok(Identifier("*".to_string()))),
            '/' => Some(Ok(Identifier("/".to_string()))),
            c if c.is_ascii_digit() => Some(self.number()),
            c if c.is_ascii_alphabetic() => Some(self.identifier()),
            c => Some(Err(UnexpectedCharacter(c)))
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace)
    }

    fn number(&mut self) -> Result<Token, SyntaxError> {
        self.advance_while(char::is_ascii_digit);
        
        if self.advance_if(|&c| c == '.').is_some() {
            self.advance_while(char::is_ascii_digit);
        }

        Ok(Numeric(self.lexeme().to_string()))
    }

    fn identifier(&mut self) -> Result<Token, SyntaxError> {
        self.advance_while(char::is_ascii_alphanumeric);

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

pub fn scan(source_code: &str) -> (Vec<Token>, Vec<SyntaxError>) {
    let mut scanner = Scanner::new(source_code);
    scanner.scan()
}
