use crate::location::Span;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug)]
pub enum LexicalErrorKind {
    MissingCharacter,
    ExpectedCharacter(char),
    UnexpectedCharacter(char),
    InvalidEscape(char),
}

#[derive(Debug)]
pub struct LexicalError {
    pub kind: LexicalErrorKind,
    pub span: Span
}

impl LexicalError {
    fn new(kind: LexicalErrorKind, start: u16, end: u16) -> Self {
        Self { kind, span: Span { start, end } }
    }
}

#[derive(Debug)]
pub enum TokenKind {
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

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

impl Token {
    fn new(kind: TokenKind, start: u16, end: u16) -> Self {
        Self { kind, span: Span { start, end } }
    }
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
    fn tokenize(&mut self) -> Result<Vec<Token>, Vec<LexicalError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while let Some((start, c)) = self.advance() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '[' => tokens.push(Token::new(TokenKind::OpenBracket, start, start + 1)),
                ']' => tokens.push(Token::new(TokenKind::CloseBracket, start, start + 1)),
                '(' => tokens.push(Token::new(TokenKind::OpenParen, start, start + 1)),
                ')' => tokens.push(Token::new(TokenKind::CloseParen, start, start + 1)),
                '{' => tokens.push(Token::new(TokenKind::OpenBrace, start, start + 1)),
                '}' => tokens.push(Token::new(TokenKind::CloseBrace, start, start + 1)),
                '+' => tokens.push(Token::new(TokenKind::Plus, start, start + 1)),
                '-' => {
                    if self.advance_if_eq(&'>') {
                        tokens.push(Token::new(TokenKind::MinusGreater, start, start + 2))
                    } else {
                        tokens.push(Token::new(TokenKind::Minus, start, start + 1))
                    }
                },
                '*' => tokens.push(Token::new(TokenKind::Star, start, start + 1)),
                '/' => tokens.push(Token::new(TokenKind::Slash, start, start + 1)),
                ',' => tokens.push(Token::new(TokenKind::Comma, start, start + 1)),
                ':' => tokens.push(Token::new(TokenKind::Colon, start, start + 1)),
                '0'..='9' => {
                    let end = start + self.advance_while(char::is_ascii_digit) + 1;

                    if self.advance_if_eq(&'.') {
                        let end = end + self.advance_while(char::is_ascii_digit) + 2;
                        tokens.push(Token::new(TokenKind::Float, start, end))
                    } else {
                        tokens.push(Token::new(TokenKind::Int, start, end))
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
                                        tokens.push(Token::new(TokenKind::Char, start, escape_char_start + 2))
                                    } else {
                                        errors.push(LexicalError::new(LexicalErrorKind::ExpectedCharacter('\''), escape_char_start + 1, escape_char_start + 2))
                                    }
                                },
                                Some((escape_char_start, c)) => errors.push(LexicalError::new(LexicalErrorKind::InvalidEscape(c), escape_char_start, escape_char_start + 1)),
                                None => errors.push(LexicalError::new(LexicalErrorKind::MissingCharacter, backslash_start + 1, backslash_start + 2)),
                            }
                        }
                        Some((char_start, _)) => {
                            if self.advance_if_eq(&'\'') {
                                tokens.push(Token::new(TokenKind::Char, start, char_start + 2))
                            } else {
                                errors.push(LexicalError::new(LexicalErrorKind::ExpectedCharacter('\''), char_start + 1, char_start + 2))
                            }
                        },
                        None => errors.push(LexicalError::new(LexicalErrorKind::MissingCharacter, start + 1, start + 2))
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some((string_end, '"')) => {
                                tokens.push(Token::new(TokenKind::String, start, string_end + 1));
                                break;
                            }
                            Some((backslash_start, '\\')) => match self.advance() {
                                Some((_, 'n')) |
                                Some((_, 'r')) |
                                Some((_, 't')) |
                                Some((_, '"')) => {}
                                Some((escape_char_start, c)) => errors.push(LexicalError::new(LexicalErrorKind::InvalidEscape(c), escape_char_start, escape_char_start + 1)),
                                None => {
                                    errors.push(LexicalError::new(LexicalErrorKind::MissingCharacter, backslash_start + 1, backslash_start + 2));
                                    break;
                                }
                            },
                            Some(_) => {}
                            None => {
                                errors.push(LexicalError::new(LexicalErrorKind::MissingCharacter, start + 1, start + 2));
                                break;
                            }
                        }
                    }
                }
                c if c.is_ascii_alphabetic() => {
                    let end = start + self.advance_while(|next| matches!(next, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '_')) + 1;

                    match &self.code[start as usize..end as usize] {
                        "fn" => tokens.push(Token::new(TokenKind::Fn, start, end)),
                        _ => tokens.push(Token::new(TokenKind::Name, start, end))
                    }
                }
                _ => errors.push(LexicalError::new(LexicalErrorKind::UnexpectedCharacter(c), start, start + 1))
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn advance(&mut self) -> Option<(u16, char)> {
        self.chars.next().map(|(i, c)| (i as u16, c))
    }

    fn advance_if_eq(&mut self, expected: &char) -> bool {
        self.chars.next_if(|(_, c)| c == expected).is_some()
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool) -> u16 {
        std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count() as u16
    }
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, Vec<LexicalError>> {
    let mut lexer = Lexer::new(code);
    lexer.tokenize()
}
