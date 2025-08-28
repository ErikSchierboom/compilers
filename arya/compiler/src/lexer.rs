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
    Comment,

    // Synthetic
    EndOfFile,
}

pub type LexTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

struct Lexer<TChars>
where
    TChars: Iterator<Item = char>,
{
    chars: Peekable<TChars>,
    start: u32,
    length: u16,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item = char>,
{
    fn new(source_code: TChars) -> Self {
        Self {
            chars: source_code.peekable(),
            start: 0,
            length: 0,
        }
    }

    fn lex_token(&mut self) -> LexTokenResult {
        self.skip_whitespace();

        self.start += self.length as u32;
        self.length = 0;

        match self.next_char() {
            None => self.make_token(Token::EndOfFile),
            Some(c) => match c {
                '[' => self.make_token(Token::OpenBracket),
                ']' => self.make_token(Token::CloseBracket),
                '+' => self.make_token(Token::Plus),
                '-' => self.make_token(Token::Minus),
                '*' => self.make_token(Token::Star),
                '/' => self.make_token(Token::Slash),
                '^' => self.make_token(Token::Caret),
                '&' => self.make_token(Token::Ampersand),
                '|' => self.make_token(Token::Pipe),
                '_' => self.make_token(Token::Underscore),
                '=' => self.make_token(Token::Equal),
                ':' => self.make_token(Token::Colon),
                '!' => {
                    if self.next_char_matches('=') {
                        self.make_token(Token::NotEqual)
                    } else {
                        self.make_token(Token::Bang)
                    }
                }
                '>' => {
                    if self.next_char_matches('=') {
                        self.make_token(Token::GreaterEqual)
                    } else {
                        self.make_token(Token::Greater)
                    }
                }
                '<' => {
                    if self.next_char_matches('=') {
                        self.make_token(Token::LessEqual)
                    } else {
                        self.make_token(Token::Less)
                    }
                }
                '#' => self.lex_comment(),
                '\n' => self.make_token(Token::Newline),
                c if c.is_ascii_digit() => self.lex_number(),
                c if c.is_ascii_alphabetic() => self.lex_identifier(),
                c => self.make_error(LexError::UnexpectedCharacter(c)),
            },
        }
    }

    fn lex_number(&mut self) -> LexTokenResult {
        self.next_chars_while(char::is_ascii_digit);
        self.make_token(Token::Number)
    }

    fn lex_identifier(&mut self) -> LexTokenResult {
        self.next_chars_while(char::is_ascii_alphanumeric);
        self.make_token(Token::Identifier)
    }

    fn lex_comment(&mut self) -> LexTokenResult {
        self.next_chars_while(|&c| c != '\n');
        self.make_token(Token::Comment)
    }

    fn skip_whitespace(&mut self) {
        self.next_chars_while(|&c| c != '\n' && c.is_ascii_whitespace());
    }

    fn make_token(&mut self, token: Token) -> LexTokenResult {
        Ok(self.spanned(token))
    }

    fn make_error(&mut self, error: LexError) -> LexTokenResult {
        Err(self.spanned(error))
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

    fn next_chars_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while self.next_char_if(&predicate).is_some() {}
    }

    fn next_char_matches(&mut self, expected: char) -> bool {
        self.next_char_if(|&c| c == expected).is_some()
    }

    fn next_char_if(&mut self, predicate: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(predicate).inspect(|_| self.length += 1)
    }
}

impl<TChars> Iterator for Lexer<TChars>
where
    TChars: Iterator<Item = char>,
{
    type Item = LexTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_token() {
            Ok(Spanned {
                value: Token::EndOfFile,
                ..
            }) => None,
            token_result => Some(token_result),
        }
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item = LexTokenResult> + '_ {
    Lexer::new(source.chars())
}
