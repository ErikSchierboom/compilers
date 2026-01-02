use crate::location::{Span, Spanned};
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
    Word,
    QuotedWord,

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

        while let Some((c, span)) = self.advance() {
            match c {
                ' ' | '\t' | '\r' | '\n' => continue,
                '[' => tokens.push(Spanned(Token::OpenBracket, span)),
                ']' => tokens.push(Spanned(Token::CloseBracket, span)),
                '(' => tokens.push(Spanned(Token::OpenParen, span)),
                ')' => tokens.push(Spanned(Token::CloseParen, span)),
                '\'' => {
                    let span = self.advance_while(is_word_character, &span);
                    tokens.push(Spanned(Token::QuotedWord, span))
                }
                '0'..='9' => {
                    let span = self.advance_while(char::is_ascii_digit, &span);

                    if let Some(span) = self.advance_if_eq(&'.', &span) {
                        let span = self.advance_while(char::is_ascii_digit, &span);
                        tokens.push(Spanned(Token::Float, span))
                    } else {
                        tokens.push(Spanned(Token::Int, span))
                    }
                }
                '#' => {
                    match self.advance() {
                        Some(('\\', span)) => {
                            match self.advance() {
                                Some(('n', span)) |
                                Some(('r', span)) |
                                Some(('t', span)) |
                                Some(('\'', span)) => tokens.push(Spanned(Token::Char, span)),
                                Some((c, span)) => errors.push(Spanned(LexError::InvalidEscape(c), span)),
                                None => errors.push(Spanned(LexError::ExpectedCharacter, span)),
                            }
                        }
                        Some((_, span)) => tokens.push(Spanned(Token::Char, span)),
                        None => errors.push(Spanned(LexError::ExpectedCharacter, span.next_position()))
                    }
                }
                '"' => {
                    loop {
                        match self.advance() {
                            Some(('"', span)) => {
                                tokens.push(Spanned(Token::String, span));
                                break;
                            }
                            Some(('\\', _)) => match self.advance() {
                                Some(('n', _)) |
                                Some(('r', _)) |
                                Some(('t', _)) |
                                Some(('"', _)) => {}
                                Some((c, escape_span)) => errors.push(Spanned(LexError::InvalidEscape(c), escape_span)),
                                None => {
                                    errors.push(Spanned(LexError::ExpectedCharacter, span.clone()));
                                    break;
                                }
                            },
                            Some(_) => {}
                            None => {
                                errors.push(Spanned(LexError::ExpectedCharacter, span.next_position()));
                                break;
                            }
                        }
                    }
                }
                c if is_word_character(&c) => {
                    let span = self.advance_while(is_word_character, &span);
                    tokens.push(Spanned(Token::Word, span))
                }
                _ => errors.push(Spanned(LexError::UnexpectedCharacter(c), span))
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn advance(&mut self) -> Option<(char, Span)> {
        self.chars.next().map(|(start, c)| (c, Span { start, end: start + 1 }))
    }

    fn advance_if_eq(&mut self, expected: &char, location: &Span) -> Option<Span> {
        self.chars.next_if(|(_, c)| c == expected).map(|_| {
            Span {
                start: location.start,
                end: location.end + 1,
            }
        })
    }

    fn advance_while(&mut self, f: impl Fn(&char) -> bool, location: &Span) -> Span {
        Span {
            start: location.start,
            end: location.end + std::iter::from_fn(|| self.chars.next_if(|(_, c)| f(c))).count(),
        }
    }
}

fn is_word_character(c: &char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..= '9' | '.' | '<' | '>' | '=' | '?' | '$' | '%' | '!' | '+' | '-' | '*' | '/' | '&' | '^')
}

pub fn tokenize(code: &str) -> Result<Vec<Spanned<Token>>, Vec<Spanned<LexError>>> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
