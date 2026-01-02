use crate::location::Spanned;
use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub enum LexError {
    ExpectedCharacter,
    UnexpectedCharacter(char),
    InvalidEscape(char),
}

#[derive(Clone, Debug)]
pub enum Token {
    // Literals
    Int,
    Float,
    Char,
    String,
    Quote,
    Word,

    // Delimiters
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
}

struct Lexer<T: Iterator<Item=char>> {
    chars: Peekable<Enumerate<T>>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.enumerate().peekable() }
    }

    // TODO: lex whitespace
    fn tokenize(&mut self) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexError>>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some((start, c, end)) = self.advance() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '[' => tokens.push(Spanned::new(Token::OpenBracket, start, end)),
                ']' => tokens.push(Spanned::new(Token::CloseBracket, start, end)),
                '(' => tokens.push(Spanned::new(Token::OpenParen, start, end)),
                ')' => tokens.push(Spanned::new(Token::CloseParen, start, end)),
                '\'' => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Spanned::new(Token::Quote, start, end))
                }
                '0'..='9' => {
                    let end = self.advance_while(char::is_ascii_digit, start);

                    if self.advance_if_eq(&'.') {
                        let end = self.advance_while(char::is_ascii_digit, end + 1);
                        tokens.push(Spanned::new(Token::Float, start, end))
                    } else {
                        tokens.push(Spanned::new(Token::Int, start, end))
                    }
                }
                '#' => {
                    match self.advance() {
                        Some((start, '\\', end)) => {
                            match self.advance() {
                                Some((_, 'n', end)) |
                                Some((_, 'r', end)) |
                                Some((_, 't', end)) |
                                Some((_, '\'', end)) => tokens.push(Spanned::new(Token::Char, start, end)),
                                Some((start, c, end)) => errors.push(Spanned::new(LexError::InvalidEscape(c), start, end)),
                                None => errors.push(Spanned::new(LexError::ExpectedCharacter, start, end)),
                            }
                        }
                        Some((start, _, end)) => tokens.push(Spanned::new(Token::Char, start, end)),
                        None => errors.push(Spanned::new(LexError::ExpectedCharacter, start + 1, end + 1))
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((start, '"', end)) => tokens.push(Spanned::new(Token::String, start, end)),
                            Some((start, '\\', end)) => match self.advance() {
                                Some((_, 'n', _)) |
                                Some((_, 'r', _)) |
                                Some((_, 't', _)) |
                                Some((_, '"', _)) => {}
                                Some((start, c, end)) => errors.push(Spanned::new(LexError::InvalidEscape(c), start, end)),
                                None => errors.push(Spanned::new(LexError::ExpectedCharacter, start, end)),
                            },
                            Some(_) |
                            None => errors.push(Spanned::new(LexError::ExpectedCharacter, start + 1, end + 1)),
                        }
                    }
                }
                c if Self::is_word_character(&c) => {
                    let end = self.advance_while(Self::is_word_character, start);
                    tokens.push(Spanned::new(Token::Word, start, end))
                }
                _ => errors.push(Spanned::new(LexError::UnexpectedCharacter(c), start, end))
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn is_word_character(c: &char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
    }

    fn advance(&mut self) -> Option<(usize, char, usize)> {
        self.chars.next().map(|(start, c)| (start, c, start + 1))
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool, start: usize) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count() + start + 1
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexError>>> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
