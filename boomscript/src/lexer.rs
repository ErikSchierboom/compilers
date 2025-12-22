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
        let mut lexer = Self { chars: code.enumerate().peekable(), char: None, pos: 0, tokens: Vec::new() };
        lexer.next_char();
        lexer
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

    fn emit(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn eat_single_char(&mut self, token: Token) {
        self.next_char();
        self.emit(token)
    }

    fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while let Some(char) = self.char {
            match char {
                ' ' | '\r' | '\n' | '\t' => {
                    self.next_char();
                }
                '+' => self.eat_single_char(Token::Add),
                '*' => self.eat_single_char(Token::Mul),
                '@' => self.eat_single_char(Token::Read(None)),
                '%' => self.eat_single_char(Token::Write(None)),
                '!' => self.eat_single_char(Token::Execute(None)),
                '[' => self.eat_single_char(Token::OpenBracket),
                ']' => self.eat_single_char(Token::CloseBracket),
                '(' => self.eat_single_char(Token::OpenParen),
                ')' => self.eat_single_char(Token::CloseParen),
                '\'' => {
                    let mut word = String::new();
                    word.push(char);

                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        word.push(char);
                    }
                    if word.is_empty() {
                        return Err(LexError::ExpectedIdentifier);
                    }

                    self.eat_single_char(Token::Quote(word))
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(char);

                    while let Some(char) = self.next_char_if(char::is_ascii_digit) {
                        number.push(char);
                    }

                    self.eat_single_char(Token::Int(number.parse().unwrap()))
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut name = String::new();
                    name.push(char);

                    while let Some(char) = self.next_char_if(char::is_ascii_alphanumeric) {
                        name.push(char);
                    }

                    // TODO: add separate function to convert string to keyword
                    match &name[..] {
                        "dup" => self.eat_single_char(Token::Dup),
                        "drop" => self.eat_single_char(Token::Drop),
                        "swap" => self.eat_single_char(Token::Swap),
                        "over" => self.eat_single_char(Token::Over),
                        _ => self.eat_single_char(Token::Identifier(name)),
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
