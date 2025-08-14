use std::iter::Peekable;

#[derive(Debug)]
pub struct Span {
    pub position: u32,
    pub length: u16
}

impl Span {
    pub fn new(position: u32, length: u16) -> Self {
        Self { position, length }
    }
}

#[derive(Debug)]
pub enum LexErrorType {
    UnexpectedCharacter(char),
    InvalidCharacterEscape(char),
    MissingCharacterValue,
    UnterminatedString
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorType,
    pub span: Span
}

#[derive(Debug)]
pub enum TokenKind {
    Number,
    Character,
    String,
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
        self.chars.next_if(func).inspect(|&c| {
            self.position += 1
        } )
    }
}

struct Lexer<T> where T: Iterator<Item = char> {
    chars: CharacterWindow<T>,
    start: u32
}

pub type LexResult = Result<Token, LexError>;

impl<T> Lexer<T> where T : Iterator<Item = char> {
    pub fn new(source: T) -> Self {
        let chars = CharacterWindow::new(source);
        let start = 0;
        Lexer { chars, start }
    }

    fn lex_token(&mut self) -> Option<LexResult> {
        self.skip_whitespace();
        self.skip_comment();

        self.start = self.chars.position;

        match self.chars.advance()? {
            '[' => Some(Ok(self.token(TokenKind::OpenBracket))),
            ']' => Some(Ok(self.token(TokenKind::CloseBracket))),
            '+' => Some(Ok(self.token(TokenKind::Plus))),
            '-' => Some(Ok(self.token(TokenKind::Minus))),
            '*' => Some(Ok(self.token(TokenKind::Star))),
            '/' => Some(Ok(self.token(TokenKind::Slash))),
            '@' => self.character(),
            '"' => self.string(),
            c if c.is_ascii_digit() => self.number(),
            c => Some(Err(self.error(LexErrorType::UnexpectedCharacter(c))))
        }
    }

    fn number(&mut self) -> Option<LexResult> {
        self.chars.advance_while(char::is_ascii_digit);
        Some(Ok(self.token(TokenKind::Number)))
    }

    fn character(&mut self) -> Option<LexResult> {
        match self.char() {
            Some(Ok(_)) => Some(Ok(self.token(TokenKind::Character))),
            Some(Err(error)) => Some(Err(error)),
            None => Some(Err(self.error(LexErrorType::MissingCharacterValue)))
        }
    }

    fn string(&mut self) -> Option<LexResult> {
        loop {
            match self.char() {
                Some(Ok('"')) => return Some(Ok(self.token(TokenKind::String))),
                Some(Ok(_)) => {},
                Some(Err(error)) => return Some(Err(error)),
                None => return Some(Err(self.error(LexErrorType::UnterminatedString)))
            }
        }
    }

    fn char(&mut self) -> Option<Result<char, LexError>> {
        match self.chars.advance() {
            Some('\\') => {
                match self.chars.advance() {
                    Some(c) => match c {
                        'n' | 'r' | 't' | '\\' | 'b' | '0' => Some(Ok(c)),
                        _ => Some(Err(self.error(LexErrorType::InvalidCharacterEscape(c)))),
                    },
                    None => Some(Err(self.error(LexErrorType::MissingCharacterValue)))
                }
            },
            Some(c) => Some(Ok(c)),
            None => None
        }
    }

    fn token(&mut self, kind: TokenKind) -> Token {
        Token { kind, span: self.span() }
    }

    fn error(&mut self, kind: LexErrorType) -> LexError {
        LexError { kind, span: self.span() }
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
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item=LexResult> + '_ {
    Lexer::new(source.chars())
}
