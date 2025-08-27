use crate::location::{Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Clone, Debug)]
pub enum LexError {
    UnexpectedCharacter(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedCharacter(c) => write!(f, "Unexpected character '{c}'"),
        }
    }
}

impl Error for LexError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // TODO: String
    // TODO: Char
    Number,
    Identifier,

    // Delimiters
    OpenBracket,
    CloseBracket,

    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Ampersand,
    Pipe,
    Bang,
    Underscore,
    Colon,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Trivia
    Newline,
    Whitespace,
    Comment,

    // Synthetic
    EndOfFile,
}

pub type LexResult = Result<Spanned<Token>, Spanned<LexError>>;

struct Lexer<T>
where
    T: Iterator<Item = char>,
{
    chars: Peekable<T>,
    start: u32,
    length: u16,
}

impl<T> Lexer<T>
where
    T: Iterator<Item = char>,
{
    fn new(source_code: T) -> Self {
        Self {
            chars: source_code.peekable(),
            start: 0,
            length: 0,
        }
    }

    fn token(&mut self) -> LexResult {
        self.start += self.length as u32;
        self.length = 0;

        match self.next_char() {
            None => self.spanned_token(Token::EndOfFile),
            Some(c) => match c {
                '[' => self.spanned_token(Token::OpenBracket),
                ']' => self.spanned_token(Token::CloseBracket),
                '+' => self.spanned_token(Token::Plus),
                '-' => self.spanned_token(Token::Minus),
                '*' => self.spanned_token(Token::Star),
                '/' => self.spanned_token(Token::Slash),
                '^' => self.spanned_token(Token::Caret),
                '&' => self.spanned_token(Token::Ampersand),
                '|' => self.spanned_token(Token::Pipe),
                '_' => self.spanned_token(Token::Underscore),
                '=' => self.spanned_token(Token::Equal),
                ':' => self.spanned_token(Token::Colon),
                '!' => {
                    if self.next_char_matches('=') {
                        self.spanned_token(Token::NotEqual)
                    } else {
                        self.spanned_token(Token::Bang)
                    }
                }
                '>' => {
                    if self.next_char_matches('=') {
                        self.spanned_token(Token::GreaterEqual)
                    } else {
                        self.spanned_token(Token::Greater)
                    }
                }
                '<' => {
                    if self.next_char_matches('=') {
                        self.spanned_token(Token::LessEqual)
                    } else {
                        self.spanned_token(Token::Less)
                    }
                }
                '#' => self.comment(),
                '\n' => self.spanned_token(Token::Newline),
                c if c.is_ascii_whitespace() => self.whitespace(),
                c if c.is_ascii_digit() => self.number(),
                c if c.is_ascii_alphabetic() => self.identifier(),
                c => self.spanned_error(LexError::UnexpectedCharacter(c)),
            },
        }
    }

    fn number(&mut self) -> LexResult {
        self.next_chars_while(char::is_ascii_digit);
        self.spanned_token(Token::Number)
    }

    fn identifier(&mut self) -> LexResult {
        self.next_chars_while(char::is_ascii_alphanumeric);
        self.spanned_token(Token::Identifier)
    }

    fn spanned_token(&mut self, token: Token) -> LexResult {
        Ok(self.spanned(token))
    }

    fn spanned_error(&mut self, error: LexError) -> LexResult {
        Err(self.spanned(error))
    }

    fn whitespace(&mut self) -> LexResult {
        self.next_chars_while(|&c| c != '\n' && c.is_ascii_whitespace());
        self.spanned_token(Token::Whitespace)
    }

    fn comment(&mut self) -> LexResult {
        self.next_chars_while(|&c| c != '\n');
        self.spanned_token(Token::Comment)
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span())
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.length)
    }

    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }

    fn next_chars_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.next_char_if(&func).is_some() {}
    }

    fn next_char_matches(&mut self, expected: char) -> bool {
        self.next_char_if(|&c| c == expected).is_some()
    }

    fn next_char_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.length += 1)
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.token() {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => None,
            token_result => Some(token_result),
        }
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item = LexResult> + '_ {
    Lexer::new(source.chars())
}
