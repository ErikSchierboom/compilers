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
pub struct Spanned<T> {
    pub value: T,
    pub span: Span
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Clone, Debug)]
pub enum LexError {
    UnexpectedCharacter(char),
    InvalidCharacterEscape(char),
    MissingCharacterValue,
    UnterminatedString
}

#[derive(Debug)]
pub enum Token {
    Number,
    OpenBracket,
    CloseBracket,
    Plus,
    Minus,
    Star,
    Slash,
}

struct CharacterWindow<T: Iterator<Item = char>> {
    chars: Peekable<T>,
    position: u32
}

impl<T> CharacterWindow<T> where T : Iterator<Item = char> {
    fn new(chars: T) -> Self {
        CharacterWindow { chars: chars.peekable(), position: 0 }
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_if(|_| true)
    }

    fn advance_if_match(&mut self, expected: &char) -> Option<char> {
        self.advance_if(|c| c == expected)
    }

    fn advance_while(&mut self, func: impl Fn(&char) -> bool) {
        while self.advance_if(&func).is_some() {}
    }

    fn advance_if(&mut self, func: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(func).inspect(|_| {
            self.position += 1
        })
    }
}

struct Lexer<T> where T: Iterator<Item = char> {
    chars: CharacterWindow<T>,
    start: u32
}

pub type ParseTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

impl<T> Lexer<T> where T : Iterator<Item = char> {
    fn new(source_code: T) -> Self {
        let chars = CharacterWindow::new(source_code);
        let start = 0;
        Lexer { chars, start }
    }

    fn parse_token(&mut self) -> Option<ParseTokenResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.start = self.chars.position;

        match self.chars.advance()? {
            '[' => self.token(Token::OpenBracket),
            ']' => self.token(Token::CloseBracket),
            '+' => self.token(Token::Plus),
            '-' => self.token(Token::Minus),
            '*' => self.token(Token::Star),
            '/' => self.token(Token::Slash),
            c if c.is_ascii_digit() => self.number(),
            c => self.error(LexError::UnexpectedCharacter(c))
        }
    }

    fn number(&mut self) -> Option<ParseTokenResult> {
        self.chars.advance_while(char::is_ascii_digit);
        self.token(Token::Number)
    }

    fn token(&mut self, token: Token) -> Option<ParseTokenResult> {
        Some(Ok(self.spanned(token)))
    }

    fn error(&mut self, error: LexError) -> Option<ParseTokenResult> {
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

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span())
    }

    fn span(&self) -> Span {
        Span::new(self.start, (self.chars.position - self.start) as u16)
    }
}

impl<T> Iterator for Lexer<T> where T : Iterator<Item = char> {
    type Item = ParseTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item=ParseTokenResult> + '_ {
    Lexer::new(source.chars())
}
