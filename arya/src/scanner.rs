use crate::source::{Location, Source, Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Clone, Debug)]
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
            ScanError::ExpectedCharacter(chars) => write!(f, "Expected one of {chars:?}"),
            ScanError::InvalidEscape(c) => write!(f, "Invalid escape \\{c:?}")
        }
    }
}

impl Error for ScanError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Number,
    Character,
    String,
    Identifier,
    OpenBracket,
    CloseBracket
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number => write!(f, "number"),
            Token::Character => write!(f, "character"),
            Token::String => write!(f, "string"),
            Token::Identifier => write!(f, "identifier"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]")
        }
    }
}

struct CharacterWindow<'a> {
    chars: Peekable<std::str::Chars<'a>>,
    location: Location
}

impl<'a> CharacterWindow<'a> {
    pub fn new(source: &'a Source) -> Self {
        let chars = source.source_code().chars().peekable();
        let location = Location::new();
        CharacterWindow { chars, location }
    }

    pub fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    pub fn advance_if_match(&mut self, expected: &char) -> Option<char> {
        self.advance_if(|c| c == expected)
    }

    pub fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|&c| {
            self.location.advance(c)
        } )
    }

    pub fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }
}

struct Scanner<'a> {
    chars: CharacterWindow<'a>,
    start: Location,
    source: &'a Source
}

type ScanTokenResult = Result<Spanned<Token>, Spanned<ScanError>>;
type ScanResult = Result<Vec<Spanned<Token>>, Vec<Spanned<ScanError>>>;

impl<'a> Scanner<'a> {
    pub fn new(source: &'a Source) -> Self {
        let chars = CharacterWindow::new(source);
        let start = chars.location.clone();

        Scanner { chars, start, source }
    }

    pub fn scan(&mut self) -> ScanResult {
        let mut tokens: Vec<Spanned<Token>> = Vec::new();
        let mut errors: Vec<Spanned<ScanError>> = Vec::new();

        while let Some(scan_token_result) = self.scan_token() {
            match scan_token_result {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error)
            }
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(tokens)
        }
    }

    fn scan_token(&mut self) -> Option<ScanTokenResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.start = self.chars.location.clone();

        match self.chars.advance()? {
            '[' => self.token(Token::OpenBracket),
            ']' => self.token(Token::CloseBracket),
            '+' | '-' | '*' | '/' => self.identifier(),
            '@' => self.character(),
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() => self.identifier(),
            c => self.error(ScanError::UnexpectedCharacter(c))
        }
    }

    fn identifier(&mut self) -> Option<ScanTokenResult> {
        self.chars.advance_while(char::is_ascii_alphanumeric);
        self.token(Token::Identifier)
    }

    fn number(&mut self) -> Option<ScanTokenResult> {
        self.chars.advance_while(char::is_ascii_digit);
        self.token(Token::Number)
    }

    fn character(&mut self) -> Option<ScanTokenResult> {
        match self.char() {
            Some(Ok(_)) => self.token(Token::Character),
            Some(Err(error)) => self.error(error),
            None => self.error(ScanError::ExpectedCharacter(vec![]))
        }
    }

    fn string(&mut self) -> Option<ScanTokenResult> {
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
        match self.chars.advance() {
            Some('\\') => {
                match self.chars.advance() {
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

    fn token(&mut self, token: Token) -> Option<ScanTokenResult> {
        Some(Ok(self.spanned(token)))
    }

    fn error(&mut self, error: ScanError) -> Option<ScanTokenResult> {
        Some(Err(self.spanned(error)))
    }

    fn skip_whitespace(&mut self) {
        self.chars.advance_while(|&c| c.is_whitespace())
    }

    fn skip_comment(&mut self) {
        if self.chars.advance_if_match(&'#').is_some() {
            self.chars.advance_while(|&c| c != '\n');
            self.chars.advance();
        }
    }

    fn spanned<T>(&self, value: T) -> Spanned<T> {
        let source = self.source.clone();
        Spanned::new(value, Span::new(source, self.start.clone(), self.chars.location.clone()))
    }
}

pub fn scan(source: &Source) -> ScanResult {
    let mut scanner = Scanner::new(source);
    scanner.scan()
}
