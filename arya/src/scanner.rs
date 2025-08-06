use std::iter::Peekable;

#[derive(Debug)]
pub enum ScanError {
    UnexpectedCharacter,
    ExpectedCharacter(Vec<char>),
    InvalidEscape(char)
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
    String,
    Identifier,
    OpenBracket,
    CloseBracket,
    Primitive
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

        match self.advance()? {
            '[' => self.token(Token::OpenBracket),
            ']' => self.token(Token::CloseBracket),
            '+' | '-' | '*' | '/' => self.token(Token::Primitive),
            '@' => self.character(),
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c if c.is_ascii_alphabetic() => self.identifier(),
            _ => self.error(ScanError::UnexpectedCharacter)
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
        // TODO: escape characters
        
        match self.advance() {
            Some(_) => Some(Ok(self.spanned(Token::Character))),
            None => Some(Err(self.spanned(ScanError::ExpectedCharacter(vec![]))))
        }
    }

    fn string(&mut self) -> Option<ScanResult> {
        // TODO: escape characters

        self.advance_while(|&c| c != '"');
     
        match self.advance_if_match(&'"') {
            Some(_) => self.token(Token::String),
            None => self.error(ScanError::ExpectedCharacter(vec!['"']))
        }
    }
    
    fn char(&mut self) -> Option<Result<char, ScanError>> {
        match self.advance() {
            Some('\\') => {
                match self.advance() {
                    Some(c) => Some(Err(ScanError::InvalidEscape(c))),
                    None => Some(Err(ScanError::ExpectedCharacter(vec!['n', 't', 'r', '\\'])))
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
