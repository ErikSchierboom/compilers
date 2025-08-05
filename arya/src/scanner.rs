// "+ [1 2 3] 2 # this is a comment"

use std::iter::Peekable;

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter,
    ExpectedCharacter(char)
}

#[derive(Clone, Debug)]
pub struct Span {
    pub begin: usize,
    pub end: usize
}

impl Span {
    pub fn empty() -> Self {
        Span { begin: 0, end: 0 }
    }

    pub fn advance(&mut self) {
        self.begin = self.end
    }
}

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
    Identifier,
    OpenBracket,
    CloseBracket
}

type ScanResult = Result<Spanned<Token>, Spanned<ScanError>>;

pub struct Scanner<'a> {
    source_code: &'a str,
    chars: Peekable<std::str::Chars<'a>>,
    span: Span
}

impl<'a> Scanner<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Scanner { source_code, chars: source_code.chars().peekable(), span: Span::empty() }
    }

    fn scan_token(&mut self) -> Option<ScanResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.span.advance();

        match self.chars.next()? {
            '[' => Some(Ok(self.spanned(Token::OpenBracket))),
            ']' => Some(Ok(self.spanned(Token::CloseBracket))),
            _ => Some(Err(self.spanned(ScanError::UnexpectedCharacter)))
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(|&c| c.is_whitespace())
    }

    fn skip_comment(&mut self) {
        if self.advance_if_match(&'#').is_some() {
            self.advance_while(|&c| c != '\n')
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

pub fn scan(source_code: &str) -> Scanner {
    Scanner::new(source_code)
}