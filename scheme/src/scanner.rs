use std::iter::Peekable;
use crate::scanner::SyntaxError::{ExpectedCharacter, UnexpectedCharacter};
use crate::scanner::Token::{Bool, Identifier, Integer};

#[derive(Debug, Clone)]
pub enum SyntaxError {
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
    // TODO: float
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
                    ';' => self.skip_to_newline(),
                    '(' => self.tokens.push(Token::OpenParen),
                    ')' => self.tokens.push(Token::CloseParen),
                    '.' => self.tokens.push(Identifier(c.to_string())),
                    '+' | '-' => {
                        match self.peek() {
                            Some('0'..='9') => {
                                let integer = self.number()?;
                                let sign = if c == '+' { 1 } else { 0 };
                                self.tokens.push(Integer(sign * integer))
                            },
                            _ => self.tokens.push(Identifier(c.to_string()))
                        }
                    },
                    '0'..='9' => {
                        let integer = self.number()?;
                        self.tokens.push(Integer(integer))
                    },
                    '"' => {
                        let string = self.string()?;
                        self.tokens.push(Token::String(string))
                    }
                    '#' => {
                        match self.advance() {
                            Some('t') => self.tokens.push(Bool(true)),
                            Some('f') => self.tokens.push(Bool(false)),
                            Some('\\') => match self.advance() {
                                Some(n) => self.tokens.push(Token::Char(n)),
                                None => return Err(UnexpectedCharacter(c))
                            }
                            _ => return Err(UnexpectedCharacter(c))
                        }
                    }
                    _ => {
                        if Self::initial(&c) {
                            let identifier = self.identifier()?;
                            self.tokens.push(Token::Identifier(identifier));
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

    fn initial(c: &char) -> bool {
        c.is_ascii_alphabetic() || "!$%&*/:<=>?~_^".contains(*c)
    }

    fn subsequent(c: &char) -> bool {
        Self::initial(c) || c.is_ascii_digit() || ".+-".contains(*c)
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace)
    }

    fn skip_to_newline(&mut self) {
        self.advance_while(|&c| c != '\n')
    }

    fn number(&mut self) -> Result<i64, SyntaxError> {
        self.advance_while(char::is_ascii_digit);

        Ok(self.lexeme().parse().unwrap())
    }

    fn identifier(&mut self) -> Result<String, SyntaxError> {
        self.advance_while(Self::subsequent);

        Ok(self.lexeme().to_string())
    }

    fn string(&mut self) -> Result<String, SyntaxError> {
        loop {
            match self.advance() {
                Some('"') => break,
                Some('\\') => {
                    match self.advance() {
                        Some('\\') | Some('"') => {}
                        Some(c) => return Err(UnexpectedCharacter(c)),
                        _ => return Err(ExpectedCharacter('"'))
                    }
                },
                Some(c) => {}
                None => return Err(ExpectedCharacter('"'))
            }
        }

        Ok(self.lexeme().to_string())
    }

    fn lexeme(&mut self) -> &'a str {
        &self.source[self.start..self.current]
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
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
