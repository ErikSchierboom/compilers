use crate::location::Spanned;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::{Enumerate, Peekable};

#[derive(Debug)]
pub enum LexError {
    ExpectedCharacter,
    UnexpectedCharacter(char),
    InvalidEscape(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LexError::ExpectedCharacter => write!(f, "expected character"),
            LexError::UnexpectedCharacter(c) => write!(f, "unexpected character '{c}'"),
            LexError::InvalidEscape(c) => write!(f, "invalid escape '\\{c}'"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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

        while let Some((start, c)) = self.advance() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '[' => tokens.push(Spanned::from_range(Token::OpenBracket, start, start + 1)),
                ']' => tokens.push(Spanned::from_range(Token::CloseBracket, start, start + 1)),
                '(' => tokens.push(Spanned::from_range(Token::OpenParen, start, start + 1)),
                ')' => tokens.push(Spanned::from_range(Token::CloseParen, start, start + 1)),
                '\'' => {
                    let end = start + self.advance_while(is_word_character) + 1;
                    tokens.push(Spanned::from_range(Token::Quote, start, end))
                }
                '0'..='9' => {
                    let end = start + self.advance_while(char::is_ascii_digit) + 1;

                    if self.advance_if_eq(&'.') {
                        let end = end + self.advance_while(char::is_ascii_digit) + 2;
                        tokens.push(Spanned::from_range(Token::Float, start, end))
                    } else {
                        tokens.push(Spanned::from_range(Token::Int, start, end))
                    }
                }
                '#' => {
                    match self.advance() {
                        Some((backslash_start, '\\')) => {
                            match self.advance() {
                                Some((escape_char_start, 'n')) |
                                Some((escape_char_start, 'r')) |
                                Some((escape_char_start, 't')) |
                                Some((escape_char_start, '\'')) => tokens.push(Spanned::from_range(Token::Char, start, escape_char_start + 1)),
                                Some((escape_char_start, c)) => errors.push(Spanned::from_range(LexError::InvalidEscape(c), escape_char_start, escape_char_start + 1)),
                                None => errors.push(Spanned::from_range(LexError::ExpectedCharacter, backslash_start + 1, backslash_start + 2)),
                            }
                        }
                        Some((char_start, _)) => tokens.push(Spanned::from_range(Token::Char, start, char_start + 1)),
                        None => errors.push(Spanned::from_range(LexError::ExpectedCharacter, start + 1, start + 2))
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((string_end, '"')) => {
                                tokens.push(Spanned::from_range(Token::String, start, string_end + 1));
                                break;
                            }
                            Some((backslash_start, '\\')) => match self.advance() {
                                Some((_, 'n')) |
                                Some((_, 'r')) |
                                Some((_, 't')) |
                                Some((_, '"')) => {}
                                Some((escape_char_start, c)) => errors.push(Spanned::from_range(LexError::InvalidEscape(c), escape_char_start, escape_char_start + 1)),
                                None => {
                                    errors.push(Spanned::from_range(LexError::ExpectedCharacter, backslash_start + 1, backslash_start + 2));
                                    break;
                                }
                            },
                            Some(_) => {}
                            None => {
                                errors.push(Spanned::from_range(LexError::ExpectedCharacter, start + 1, start + 2));
                                break;
                            }
                        }
                    }
                }
                c if is_word_character(&c) => {
                    let end = start + self.advance_while(is_word_character) + 1;
                    tokens.push(Spanned::from_range(Token::Word, start, end))
                }
                _ => errors.push(Spanned::from_range(LexError::UnexpectedCharacter(c), start, start + 1))
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        self.chars.next()
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool) -> usize {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count()
    }
}

fn is_word_character(c: &char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
}

pub fn tokenize(code: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexError>>> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
