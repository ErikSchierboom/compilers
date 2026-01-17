use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(char)
}

#[derive(Debug)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen
}

struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Self {
        Self { chars: code.chars().peekable() }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexicalError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some(c) = self.chars.next() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '+' => tokens.push(Token::Plus),
                '-' => tokens.push(Token::Minus),
                '*' => tokens.push(Token::Star),
                '/' => tokens.push(Token::Slash),
                '0'..='9' => {
                    let mut number = c.to_digit(10).unwrap() as i64;

                    while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
                        number = number * 10 + c.to_digit(10).unwrap() as i64;
                    }

                tokens.push(Token::Number(number))
                }
                _ => errors.push(LexicalError::UnexpectedCharacter(c))
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, Vec<LexicalError>> {
    let mut lexer = Lexer::new(code);
    lexer.tokenize()
}
