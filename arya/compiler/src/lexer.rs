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

#[derive(Debug, PartialEq)]
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

pub type ParseTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

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

    fn next_token(&mut self) -> ParseTokenResult {
        self.start += self.length as u32;
        self.length = 0;

        match self.next_char() {
            None => self.token(Token::EndOfFile),
            Some(c) => match c {
                '[' => self.token(Token::OpenBracket),
                ']' => self.token(Token::CloseBracket),
                '+' => self.token(Token::Plus),
                '-' => self.token(Token::Minus),
                '*' => self.token(Token::Star),
                '/' => self.token(Token::Slash),
                '^' => self.token(Token::Caret),
                '&' => self.token(Token::Ampersand),
                '|' => self.token(Token::Pipe),
                '_' => self.token(Token::Underscore),
                '=' => self.token(Token::Equal),
                ':' => self.token(Token::Colon),
                '!' => {
                    if self.next_char_matches('=') {
                        self.token(Token::NotEqual)
                    } else {
                        self.token(Token::Bang)
                    }
                }
                '>' => {
                    if self.next_char_matches('=') {
                        self.token(Token::GreaterEqual)
                    } else {
                        self.token(Token::Greater)
                    }
                }
                '<' => {
                    if self.next_char_matches('=') {
                        self.token(Token::LessEqual)
                    } else {
                        self.token(Token::Less)
                    }
                }
                '#' => self.comment(),
                '\n' => self.token(Token::Newline),
                c if c.is_ascii_whitespace() => self.whitespace(),
                c if c.is_ascii_digit() => self.number(),
                c if c.is_ascii_alphabetic() => self.identifier(),
                c => self.error(LexError::UnexpectedCharacter(c)),
            },
        }
    }

    fn number(&mut self) -> ParseTokenResult {
        self.next_chars_while(char::is_ascii_digit);
        self.token(Token::Number)
    }

    fn identifier(&mut self) -> ParseTokenResult {
        self.next_chars_while(char::is_ascii_alphanumeric);
        self.token(Token::Identifier)
    }

    fn token(&mut self, token: Token) -> ParseTokenResult {
        Ok(self.spanned(token))
    }

    fn error(&mut self, error: LexError) -> ParseTokenResult {
        Err(self.spanned(error))
    }

    fn whitespace(&mut self) -> ParseTokenResult {
        self.next_chars_while(|&c| c != '\n' && c.is_ascii_whitespace());
        self.token(Token::Whitespace)
    }

    fn comment(&mut self) -> ParseTokenResult {
        self.next_chars_while(|&c| c != '\n');
        self.token(Token::Comment)
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
    type Item = ParseTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => None,
            token_result => Some(token_result),
        }
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item = ParseTokenResult> + '_ {
    Lexer::new(source.chars())
}
