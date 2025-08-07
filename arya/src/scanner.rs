use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use crate::source::{SourceText, Span};

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter(char),
    ExpectedCharacter(Vec<char>),
    InvalidEscape(char)
}

impl Display for ScanError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanError::UnexpectedCharacter(c) => write!(f, "Unexpected character {c:?}"),
            ScanError::ExpectedCharacter(chars) if chars.is_empty() => write!(f, "Expected character"),
            ScanError::ExpectedCharacter(chars) => write!(f, "Expected on of {chars:?}"),
            ScanError::InvalidEscape(c) => write!(f, "Invalid escape \\{c:?}")
        }
    }
}

impl Error for ScanError {}

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Debug)]
pub enum Token {
    Number,
    Character,
    String,
    Identifier,
    OpenBracket,
    CloseBracket,
    Primitive
}

type ScanResult = Result<Spanned<Token>, Spanned<ScanError>>;

pub struct Scanner<'a> {
    chars: Peekable<std::str::Chars<'a>>,
    span: Span
}

impl<'a> Scanner<'a> {
    pub fn new(source_text: &'a SourceText) -> Self {
        let chars = source_text.source_code.chars().peekable();
        Scanner { chars, span: Span::empty() }
    }

    fn scan_token(&mut self) -> Option<ScanResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.span.advance();

        match self.advance()? {
            '[' => self.token(Token::OpenBracket),
            ']' => self.token(Token::CloseBracket),
            '+' | '-' | '*' | '/' => self.token(Token::Primitive),
            '@' => self.character(),
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() => self.identifier(),
            c => self.error(ScanError::UnexpectedCharacter(c))
        }
    }

    fn identifier(&mut self) -> Option<ScanResult> {
        self.advance_while(char::is_ascii_alphanumeric);
        self.token(Token::Identifier)
    }

    fn number(&mut self) -> Option<ScanResult> {
        self.advance_while(char::is_ascii_digit);
        self.token(Token::Number)
    }

    fn character(&mut self) -> Option<ScanResult> {
        match self.char() {
            Some(Ok(_)) => self.token(Token::Character),
            Some(Err(error)) => self.error(error),
            None => self.error(ScanError::ExpectedCharacter(vec![]))
        }
    }

    fn string(&mut self) -> Option<ScanResult> {
        loop {
            match self.char() {
                Some(Ok('"')) => return self.token(Token::String),
                Some(Ok(_)) => {},
                Some(Err(error)) => return self.error(error),
                None => return self.error(ScanError::ExpectedCharacter(vec!['"']))
            }
        }
    }
    
    fn char(&mut self) -> Option<Result<char, ScanError>> {
        match self.advance() {
            Some('\\') => {
                match self.advance() {
                    Some(c) => match c {
                        'n' | 'r' | 't' | '\\' | 'b' | '0' => Some(Ok(c)),
                        _ => Some(Err(ScanError::InvalidEscape(c))),
                    },
                    None => Some(Err(ScanError::ExpectedCharacter(vec!['n', 't', 'r', '\\', 'b', '0'])))
                }
            },
            Some(c) => Some(Ok(c)),
            None => None
        }
    }

    fn token(&mut self, token: Token) -> Option<ScanResult> {
        Some(Ok(self.spanned(token)))
    }

    fn error(&mut self, error: ScanError) -> Option<ScanResult> {
        Some(Err(self.spanned(error)))
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(|&c| c.is_whitespace())
    }

    fn skip_comment(&mut self) {
        if self.advance_if_match(&'#').is_some() {
            self.advance_while(|&c| c != '\n');
            self.advance();
        }
    }

    fn spanned<T>(&self, value: T) -> Spanned<T> {
        Spanned::new(value, self.span.clone())
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    fn advance_if_match(&mut self, expected: &char) -> Option<char> {
        self.advance_if(|c| c == expected)
    }

    fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| self.span.end += 1 )
    }

    fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = ScanResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

pub fn scan<'a>(source_text: &'a SourceText<'a>) -> Scanner<'a> {
    Scanner::new(source_text)
}
