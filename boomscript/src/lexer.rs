use crate::location::Spanned;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug)]
pub enum LexError {
    MissingCharacter,
    ExpectedCharacter(char),
    UnexpectedCharacter(char),
    InvalidEscape(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LexError::MissingCharacter => write!(f, "missing character"),
            LexError::ExpectedCharacter(c) => write!(f, "expected character: '{c}'"),
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
    Name,

    // Operators
    Plus,
    Minus,
    MinusGreater,
    Star,
    Slash,
    Colon,
    Comma,

    // Keywords
    Fn,

    // Delimiters
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
}

struct Lexer<'a> {
    code: &'a str,
    chars: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> Lexer<'a> {
    fn new(code: &'a str) -> Self {
        let chars = code.chars().enumerate().peekable();
        Self { code, chars }
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
                '{' => tokens.push(Spanned::from_range(Token::OpenBrace, start, start + 1)),
                '}' => tokens.push(Spanned::from_range(Token::CloseBrace, start, start + 1)),
                '+' => tokens.push(Spanned::from_range(Token::Plus, start, start + 1)),
                '-' => {
                    if self.advance_if_eq(&'>') {
                        tokens.push(Spanned::from_range(Token::MinusGreater, start, start + 2))
                    } else {
                        tokens.push(Spanned::from_range(Token::Minus, start, start + 1))
                    }
                },
                '*' => tokens.push(Spanned::from_range(Token::Star, start, start + 1)),
                '/' => tokens.push(Spanned::from_range(Token::Slash, start, start + 1)),
                ',' => tokens.push(Spanned::from_range(Token::Comma, start, start + 1)),
                ':' => tokens.push(Spanned::from_range(Token::Colon, start, start + 1)),
                '0'..='9' => {
                    let end = start + self.advance_while(char::is_ascii_digit) + 1;

                    if self.advance_if_eq(&'.') {
                        let end = end + self.advance_while(char::is_ascii_digit) + 2;
                        tokens.push(Spanned::from_range(Token::Float, start, end))
                    } else {
                        tokens.push(Spanned::from_range(Token::Int, start, end))
                    }
                }
                '\'' => {
                    match self.advance() {
                        Some((backslash_start, '\\')) => {
                            match self.advance() {
                                Some((escape_char_start, 'n')) |
                                Some((escape_char_start, 'r')) |
                                Some((escape_char_start, 't')) |
                                Some((escape_char_start, '\'')) => {
                                    if self.advance_if_eq(&'\'') {
                                        tokens.push(Spanned::from_range(Token::Char, start, escape_char_start + 2))
                                    } else {
                                        errors.push(Spanned::from_range(LexError::ExpectedCharacter('\''), escape_char_start + 1, escape_char_start + 2))
                                    }
                                },
                                Some((escape_char_start, c)) => errors.push(Spanned::from_range(LexError::InvalidEscape(c), escape_char_start, escape_char_start + 1)),
                                None => errors.push(Spanned::from_range(LexError::MissingCharacter, backslash_start + 1, backslash_start + 2)),
                            }
                        }
                        Some((char_start, _)) => {
                            if self.advance_if_eq(&'\'') {
                                tokens.push(Spanned::from_range(Token::Char, start, char_start + 2))
                            } else {
                                errors.push(Spanned::from_range(LexError::ExpectedCharacter('\''), char_start + 1, char_start + 2))
                            }
                        },
                        None => errors.push(Spanned::from_range(LexError::MissingCharacter, start + 1, start + 2))
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
                                    errors.push(Spanned::from_range(LexError::MissingCharacter, backslash_start + 1, backslash_start + 2));
                                    break;
                                }
                            },
                            Some(_) => {}
                            None => {
                                errors.push(Spanned::from_range(LexError::MissingCharacter, start + 1, start + 2));
                                break;
                            }
                        }
                    }
                }
                c if c.is_ascii_alphabetic() => {
                    let end = start + self.advance_while(|next| matches!(next, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '_')) + 1;

                    match &self.code[start..end] {
                        "fn" => tokens.push(Spanned::from_range(Token::Name, start, end)),
                        _ => tokens.push(Spanned::from_range(Token::Name, start, end))
                    }
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

pub fn tokenize(code: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexError>>> {
    let mut lexer = Lexer::new(code);
    lexer.tokenize()
}
