use std::iter::Peekable;
use crate::scanner::SyntaxError::{ExpectedCharacter, MissingCharacter, UnexpectedCharacter};
use crate::scanner::Token::{Bool, Identifier, Integer};

#[derive(Debug, Clone)]
pub enum SyntaxError {
    MissingCharacter,
    UnexpectedCharacter(char),
    ExpectedCharacter(char)
}

#[derive(Debug, Clone)]
pub enum Token {
    OpenParen,
    CloseParen,
    Identifier(String),
    Integer(i64),
    Bool(bool),
    Char(char),
    String(String)
}

struct Scanner<'a> {
    source: &'a str,
    chars: Peekable<std::str::Chars<'a>>,
    tokens: Vec<Token>,
    start: usize,
    current: usize
}

impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, chars: source.chars().peekable(), tokens: Vec::new(), start: 0, current: 0 }
    }
    
    pub fn scan(&mut self) -> Result<Vec<Token>, SyntaxError> {
        loop {
            self.skip_whitespace();
            self.start = self.current;

            match self.advance() {
                Some(c) => match c {
                    ';' => self.skip_comment(),
                    '(' => self.add_token(Token::OpenParen),
                    ')' => self.add_token(Token::CloseParen),
                    '.' => self.add_token(Identifier(c.to_string())),
                    '+' | '-' => {
                        if self.advance_if(char::is_ascii_digit).is_some() {
                            let number = self.number()?;
                            self.add_token(number)
                        } else {
                            self.add_token(Identifier(c.to_string()))
                        }
                    },
                    '0'..='9' => {
                        let number = self.number()?;
                        self.add_token(number)
                    },
                    '"' => {
                        let string = self.string()?;
                        self.add_token(string)
                    }
                    '#' => {
                        match self.advance() {
                            Some('t') => self.add_token(Bool(true)),
                            Some('f') => self.add_token(Bool(false)),
                            Some('\\') => {
                                let char = self.char()?;
                                self.add_token(char);
                            }
                            _ => return Err(UnexpectedCharacter(c))
                        }
                    }
                    _ => {
                        if Self::initial(&c) {
                            let identifier = self.identifier()?;
                            self.add_token(identifier);
                        } else {
                            return Err(UnexpectedCharacter(c))
                        }
                    }
                },
                None => break
            }
        }

        Ok(self.tokens.clone())
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn initial(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^')
    }

    fn subsequent(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '.' | '+' | '-' | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^')
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace)
    }

    fn skip_comment(&mut self) {
        self.advance_while(|&c| c != '\n')
    }

    fn number(&mut self) -> Result<Token, SyntaxError> {
        self.advance_while(char::is_ascii_digit);

        Ok(Integer(self.lexeme().parse().unwrap()))
    }

    fn identifier(&mut self) -> Result<Token, SyntaxError> {
        self.advance_while(Self::subsequent);

        Ok(Identifier(self.lexeme().to_string()))
    }

    fn string(&mut self) -> Result<Token, SyntaxError> {
        loop {
            match self.advance() {
                Some('"') => break,
                Some('\\') => {
                    match self.advance() {
                        Some('\\') | Some('"') => {}
                        Some(c) => return Err(UnexpectedCharacter(c)),
                        _ => return Err(MissingCharacter)
                    }
                },
                Some(_) => {}
                None => return Err(ExpectedCharacter('"'))
            }
        }

        Ok(Token::String(self.lexeme().to_string()))
    }

    fn char(&mut self) -> Result<Token, SyntaxError> {
        match self.advance() {
            Some(c) => Ok(Token::Char(c)),
            None => Err(MissingCharacter)
        }
    }

    fn lexeme(&mut self) -> &'a str {
        &self.source[self.start..self.current]
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

pub fn scan(source_code: &str) -> Result<Vec<Token>, SyntaxError> {
    let mut scanner = Scanner::new(source_code);
    scanner.scan()
}
