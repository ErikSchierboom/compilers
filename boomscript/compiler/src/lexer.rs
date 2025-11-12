use std::iter::Peekable;
use std::string::ToString;

#[derive(Debug)]
pub enum LexError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    ExpectedCharacter(char),
    InvalidEscape(char),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Literals
    Number(i64),
    Char(char),
    String(String),
    Identifier(String),

    // Symbols
    Pipe,
}

struct Lexer<T>
where
    T: Iterator<Item=char>,
{
    chars: Peekable<T>,
}

impl<T> Lexer<T>
where
    T: Iterator<Item=char>,
{
    fn new(chars: T) -> Self {
        Self { chars: chars.peekable() }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(char) = self.chars.next() {
            if char.is_whitespace() {
                continue;
            }

            let token = match char {
                '|' => Token::Pipe,
                '\'' => {
                    let c = self.chars.next().ok_or(LexError::UnexpectedEndOfFile)?;
                    self.chars.next_if_eq(&'\'').ok_or(LexError::ExpectedCharacter('\''))?;
                    Token::Char(c)
                }
                '"' => {
                    let mut string = String::new();
                    while let Some(c) = self.chars.next_if(|&c| c != '"') {
                        string.push(c);
                    }
                    self.chars.next_if_eq(&'"').ok_or(LexError::ExpectedCharacter('"'))?;
                    Token::String(string)
                }
                c if c.is_ascii_digit() => {
                    let mut number = c.to_digit(10).unwrap();
                    while let Some(digit) = self.chars.next_if(char::is_ascii_digit) {
                        number = number * 10 + digit.to_digit(10).unwrap();
                    }
                    Token::Number(number as i64)
                }
                c if c.is_ascii_alphabetic() => {
                    let mut identifier = char.to_string();
                    while let Some(c) = self.chars.next_if(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '?')) {
                        identifier.push(c);
                    }

                    Token::Identifier(identifier)
                }
                _ => return Err(LexError::UnexpectedCharacter(char))
            };

            tokens.push(token)
        }

        Ok(tokens)
    }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source_code.chars());
    lexer.lex()
}
