use std::iter::Peekable;

#[derive(Debug)]
pub enum LexError {
    ExpectedIdentifier,
    UnknownIdentifier(String),
    UnexpectedToken(char)
}

#[derive(Clone, Debug)]
pub enum Token {
    // Literals
    Int(i64),
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
    chars: Peekable<T>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.peekable() }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while let Some(char) = self.chars.next() {
            match char {
                ' ' | '\r' | '\n' | '\t' => continue,
                '+' => tokens.push(Token::Add),
                '*' => tokens.push(Token::Mul),
                '@' => tokens.push(Token::Read(self.lex_word())),
                '%' => tokens.push(Token::Write(self.lex_word())),
                '!' => tokens.push(Token::Execute(self.lex_word())),
                '[' => tokens.push(Token::OpenBracket),
                ']' => tokens.push(Token::CloseBracket),
                '(' => tokens.push(Token::OpenParen),
                ')' => tokens.push(Token::CloseParen),
                '\'' => {
                    match self.lex_word() {
                        Some(word) => tokens.push(Token::Quote(word)),
                        None => return Err(LexError::ExpectedIdentifier)
                    }
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(char);

                    while let Some(char) = self.chars.next_if(char::is_ascii_digit) {
                        number.push(char);
                    }

                    tokens.push(Token::Int(number.parse().unwrap()))
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut name = self.lex_word().unwrap_or("".to_string());
                    name.insert(0, char);

                    match &name[..] {
                        "dup" => tokens.push(Token::Dup),
                        "drop" => tokens.push(Token::Drop),
                        "swap" => tokens.push(Token::Swap),
                        "over" => tokens.push(Token::Over),
                        _ => return Err(LexError::UnknownIdentifier(name))
                    }
                }
                _ => return Err(LexError::UnexpectedToken(char))
            }
        }

        Ok(tokens)
    }

    fn lex_word(&mut self) -> Option<String> {
        let mut word = String::new();

        while let Some(char) = self.chars.next_if(char::is_ascii_alphanumeric) {
            word.push(char);
        }

        if word.is_empty() { None } else { Some(word) }
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
