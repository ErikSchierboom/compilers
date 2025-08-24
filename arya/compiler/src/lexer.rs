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
    Number,
    Identifier,
    OpenBracket,
    CloseBracket,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Ampersand,
    Pipe,
    Bang,
    Underscore,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Dup,
    Drop,
    Swap,
    Over,
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

    fn parse_token(&mut self) -> Option<ParseTokenResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.start += self.length as u32;
        self.length = 0;

        match self.next()? {
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
            '!' => {
                if self.next_if_char('=') {
                    self.token(Token::NotEqual)
                } else {
                    self.token(Token::Bang)
                }
            }
            '>' => {
                if self.next_if_char('=') {
                    self.token(Token::GreaterEqual)
                } else {
                    self.token(Token::Greater)
                }
            }
            '<' => {
                if self.next_if_char('=') {
                    self.token(Token::LessEqual)
                } else {
                    self.token(Token::Less)
                }
            },
            'd' if self.next_if_chars("up") => self.token(Token::Dup),
            'd' if self.next_if_chars("rop") => self.token(Token::Drop),
            's' if self.next_if_chars("wap") => self.token(Token::Swap),
            'o' if self.next_if_chars("ver") => self.token(Token::Over),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() => self.identifier(),
            c => self.error(LexError::UnexpectedCharacter(c)),
        }
    }

    fn number(&mut self) -> Option<ParseTokenResult> {
        self.next_while(char::is_ascii_digit);
        self.token(Token::Number)
    }

    fn identifier(&mut self) -> Option<ParseTokenResult> {
        self.next_while(char::is_ascii_alphanumeric);
        self.token(Token::Identifier)
    }

    fn token(&mut self, token: Token) -> Option<ParseTokenResult> {
        Some(Ok(self.spanned(token)))
    }

    fn error(&mut self, error: LexError) -> Option<ParseTokenResult> {
        Some(Err(self.spanned(error)))
    }

    fn skip_whitespace(&mut self) {
        self.next_while(|&c| c.is_whitespace())
    }

    fn skip_comment(&mut self) {
        if self.next_if_char('#') {
            self.next_while(|&c| c != '\n');
            self.next(); // consume the newline character
        }
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span())
    }

    fn span(&self) -> Span {
        Span::new(self.start, self.length)
    }

    fn next(&mut self) -> Option<char> {
        self.next_if(|_| true)
    }

    fn next_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.next_if(&func).is_some() {}
    }

    fn next_if_char(&mut self, expected: char) -> bool {
        self.next_if(|c| *c == expected).is_some()
    }

    fn next_if_chars(&mut self, expected: &str) -> bool {
        expected.chars().all(|c|self.next_if_char(c))
    }

    fn next_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.length += 1)
    }
}

impl<T> Iterator for Lexer<T>
where
    T: Iterator<Item = char>,
{
    type Item = ParseTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item = ParseTokenResult> + '_ {
    Lexer::new(source.chars())
}
