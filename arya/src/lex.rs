use std::iter::Peekable;

#[derive(Clone, Debug)]
pub struct Span {
    pub position: u32,
    pub length: u16
}

impl Span {
    pub fn new(position: u32, length: u16) -> Self {
        Self { position, length }
    }

    pub fn merge(&self, rhs: &Self) -> Self {
        assert!(rhs.position >= self.position);
        Self::new(self.position, (rhs.position - self.position) as u16 + rhs.length)
    }
}

#[derive(Clone, Debug)]
pub enum LexErrorKind {
    UnexpectedCharacter(char),
    InvalidCharacterEscape(char),
    MissingCharacterValue,
    UnterminatedString
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span
}

impl LexError {
    pub fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    Number,
    OpenBracket,
    CloseBracket,
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

struct CharacterWindow<T: Iterator<Item = char>> {
    chars: Peekable<T>,
    position: u32
}

impl<T> CharacterWindow<T> where T : Iterator<Item = char> {
    pub fn new(chars: T) -> Self {
        CharacterWindow { chars: chars.peekable(), position: 0 }
    }

    pub fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    pub fn advance_if_match(&mut self, expected: &char) -> Option<char> {
        self.advance_if(|c| c == expected)
    }

    pub fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }

    pub fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| {
            self.position += 1
        })
    }
}

struct Lexer<T> where T: Iterator<Item = char> {
    chars: CharacterWindow<T>,
    start: u32
}

pub type TokenResult = Result<Token, LexError>;

impl<T> Lexer<T> where T : Iterator<Item = char> {
    pub fn new(source: T) -> Self {
        let chars = CharacterWindow::new(source);
        let start = 0;
        Lexer { chars, start }
    }

    fn next_token(&mut self) -> Option<TokenResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.start = self.chars.position;

        match self.chars.advance()? {
            '[' => self.token(TokenKind::OpenBracket),
            ']' => self.token(TokenKind::CloseBracket),
            '+' => self.token(TokenKind::Plus),
            '-' => self.token(TokenKind::Minus),
            '*' => self.token(TokenKind::Star),
            '/' => self.token(TokenKind::Slash),
            c if c.is_ascii_digit() => self.number(),
            c => self.error(LexErrorKind::UnexpectedCharacter(c))
        }
    }

    fn number(&mut self) -> Option<TokenResult> {
        self.chars.advance_while(char::is_ascii_digit);
        self.token(TokenKind::Number)
    }

    fn token(&mut self, kind: TokenKind) -> Option<TokenResult> {
        Some(Ok(Token::new(kind, self.span())))
    }

    fn error(&mut self, kind: LexErrorKind) -> Option<TokenResult> {
        Some(Err(LexError::new(kind, self.span())))
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

    fn span(&self) -> Span {
        Span::new(self.start, (self.chars.position - self.start) as u16)
    }
}

impl<T> Iterator for Lexer<T> where T : Iterator<Item = char> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item=TokenResult> + '_ {
    Lexer::new(source.chars())
}
