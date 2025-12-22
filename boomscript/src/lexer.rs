use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub enum LexError {
    ExpectedIdentifier,
    UnknownIdentifier(String),
    UnexpectedToken(char),
}


// TODO: add span

#[derive(Clone, Debug)]
pub enum Token {
    // Literals
    Int(i64),
    Identifier(String),
    Quote(String),

    // Binary operators
    Add,
    Mul,

    // Stack operators
    Dup,
    Drop,
    Swap,
    Over,

    // Memory operators
    Read(Option<String>),
    Write(Option<String>),
    Execute(Option<String>),

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
        Self { chars: code.enumerate().peekable(), char: None, pos: 0, tokens: Vec::new() }
    }

    fn next_char_if(&mut self, f: impl FnOnce(&char) -> bool) -> Option<char> {
        match self.chars.next_if(|(_, c)| f(c)) {
            Some((pos, c)) => {
                self.pos = pos;
                self.char = Some(c)
            }
            None => {
                self.pos += 1;
                self.char = None
            }
        }

        self.char
    }

    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }

    fn emit_token(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while let Some(char) = self.next_char() {
            match char {
                ' ' | '\r' | '\n' | '\t' => continue,
                '+' => self.emit_token(Token::Add),
                '*' => self.emit_token(Token::Mul),
                '@' => {
                    let mut word = String::new();
                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        word.push(char);
                    }
                    self.emit_token(Token::Read(if word.is_empty() { None } else { Some(word) }))
                }
                '%' => {
                    let mut word = String::new();
                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        word.push(char);
                    }
                    self.emit_token(Token::Write(if word.is_empty() { None } else { Some(word) }))
                }
                '!' => {
                    let mut word = String::new();
                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        word.push(char);
                    }
                    self.emit_token(Token::Execute(if word.is_empty() { None } else { Some(word) }))
                }
                '[' => self.emit_token(Token::OpenBracket),
                ']' => self.emit_token(Token::CloseBracket),
                '(' => self.emit_token(Token::OpenParen),
                ')' => self.emit_token(Token::CloseParen),
                '\'' => {
                    let mut word = String::new();
                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        word.push(char);
                    }
                    if word.is_empty() {
                        return Err(LexError::ExpectedIdentifier);
                    }

                    self.emit_token(Token::Quote(word))
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(char);

                    while let Some(char) = self.next_char_if(char::is_ascii_digit) {
                        number.push(char);
                    }

                    self.emit_token(Token::Int(number.parse().unwrap()))
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut name = String::new();
                    name.push(char);

                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        name.push(char);
                    }

                    // TODO: add separate function to convert string to keyword
                    match &name[..] {
                        "dup" => self.emit_token(Token::Dup),
                        "drop" => self.emit_token(Token::Drop),
                        "swap" => self.emit_token(Token::Swap),
                        "over" => self.emit_token(Token::Over),
                        _ => self.emit_token(Token::Identifier(name)),
                    }
                }
                _ => return Err(LexError::UnexpectedToken(char))
            }
        }

        Ok(self.tokens)
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
