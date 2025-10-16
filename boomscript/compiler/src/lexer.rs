use crate::location::{Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq)]
pub enum LexError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    ExpectedCharacter(char),
    InvalidEscape(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedEndOfFile => write!(f, "Unexpected end of file"),
            LexError::UnexpectedCharacter(c) => write!(f, "Unexpected character '{c}'"),
            LexError::ExpectedCharacter(c) => write!(f, "Expected character '{c}'"),
            LexError::InvalidEscape(c) => write!(f, "Invalid escape: '\\{c}'")
        }
    }
}

impl Error for LexError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Integer,
    Float,
    Char,
    String,
    Identifier,

    // Delimiters
    OpenBracket,
    CloseBracket,
    OpenParenthesis,
    CloseParenthesis,

    // Symbols
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    QuestionMark,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer => write!(f, "integer"),
            Token::Float => write!(f, "float"),
            Token::String => write!(f, "string"),
            Token::Identifier => write!(f, "identifier"),
            Token::Char => write!(f, "char"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Equal => write!(f, "="),
            Token::Bang => write!(f, "!"),
            Token::BangEqual => write!(f, "!="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::QuestionMark => write!(f, "?"),
        }
    }
}

pub type LexTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

struct Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    chars: Peekable<TChars>,
    char: Option<char>,
    start_pos: u32,
    current_pos: u32,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    fn new(source_code: TChars) -> Self {
        Self { chars: source_code.peekable(), char: None, start_pos: 0, current_pos: 0 }
    }

    fn lex_token(&mut self) -> Option<LexTokenResult> {
        self.skip_whitespace();

        self.start_pos = self.current_pos;
        self.advance();

        let result = match self.char? {
            '[' => Ok(Token::OpenBracket),
            ']' => Ok(Token::CloseBracket),
            '(' => Ok(Token::OpenParenthesis),
            ')' => Ok(Token::CloseParenthesis),
            '+' => {
                if self.next_char_matches(char::is_ascii_digit) {
                    self.advance();
                    self.lex_number()
                } else {
                    Ok(Token::Plus)
                }
            }
            '-' => {
                if self.next_char_matches(char::is_ascii_digit) {
                    self.advance();
                    self.lex_number()
                } else {
                    Ok(Token::Minus)
                }
            }
            '*' => Ok(Token::Star),
            '/' => Ok(Token::Slash),
            '?' => Ok(Token::QuestionMark),
            '=' => Ok(Token::Equal),
            '!' => {
                if self.next_char_is('=') {
                    self.advance();
                    Ok(Token::BangEqual)
                } else {
                    Ok(Token::Bang)
                }
            }
            '>' => {
                if self.next_char_is('=') {
                    self.advance();
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            }
            '<' => {
                if self.next_char_is('=') {
                    self.advance();
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            }
            '\'' => self.lex_char(),
            '"' => self.lex_string(),
            c if c.is_ascii_digit() => self.lex_number(),
            c if c.is_ascii_alphabetic() => self.lex_identifier(),
            _ => self.unexpected_character()
        };

        Some(match result {
            Ok(token) => Ok(self.spanned(token)),
            Err(err) => Err(self.spanned(err))
        })
    }

    fn lex_char(&mut self) -> Result<Token, LexError> {
        self.advance();
        self.lex_character()?;
        self.expect_character('\'')?;

        Ok(Token::Char)
    }

    fn lex_identifier(&mut self) -> Result<Token, LexError> {
        self.advance();
        self.advance_while_chars_match(|&c| c.is_ascii_alphanumeric() || matches!(c, '?' | '-' | '_'));

        Ok(Token::Identifier)
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        self.advance();

        while !self.next_char_is('"') {
            self.lex_character()?;
        }

        self.expect_character('"')?;

        Ok(Token::String)
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        self.advance_while_chars_match(char::is_ascii_digit);

        if self.next_char_is('.') {
            self.advance();
            self.advance_while_chars_match(char::is_ascii_digit);
            Ok(Token::Float)
        } else {
            Ok(Token::Integer)
        }
    }

    fn lex_character(&mut self) -> Result<(), LexError> {
        match self.char {
            Some('\\') => {
                self.advance();
                if let Some(c) = self.char {
                    match c {
                        'n' | 'r' | 't' | '\\' | '\'' => {
                            self.advance();
                            Ok(())
                        }
                        _ => Err(LexError::InvalidEscape(c))
                    }
                } else {
                    Err(LexError::UnexpectedEndOfFile)
                }
            }
            Some(_) => {
                self.advance();
                Ok(())
            }
            None => Err(LexError::UnexpectedEndOfFile)
        }
    }

    fn unexpected_character(&mut self) -> Result<Token, LexError> {
        match self.char {
            None => Err(LexError::UnexpectedEndOfFile),
            Some(c) => Err(LexError::UnexpectedCharacter(c))
        }
    }

    fn expect_character(&mut self, expected: char) -> Result<(), LexError> {
        if self.char == Some(expected) {
            Ok(())
        } else {
            Err(LexError::ExpectedCharacter('\''))
        }
    }

    fn advance(&mut self) {
        self.char = self.chars.next();
        self.current_pos += 1;
    }

    fn advance_while_chars_match(&mut self, predicate: impl Fn(&char) -> bool) {
        while self.next_char_matches(&predicate) {
            self.advance()
        }
    }

    fn next_char_matches(&mut self, predicate: impl FnOnce(&char) -> bool) -> bool {
        self.chars.peek().map(predicate).unwrap_or_default()
    }

    fn next_char_is(&mut self, expected: char) -> bool {
        self.next_char_matches(|c| *c == expected)
    }

    fn skip_whitespace(&mut self) {
        self.advance_while_chars_match(char::is_ascii_whitespace)
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        let span = Span::new(self.start_pos, (self.current_pos - self.start_pos) as u16);
        Spanned::new(value, span)
    }
}

impl<TChars> Iterator for Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    type Item = LexTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item=LexTokenResult> + '_ {
    let chars_with_index = source.chars();
    Lexer::new(chars_with_index)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_identifiers() {
        let mut tokens = tokenize("foo Bar BAZ read-file read_file1 empty? x2");

        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(0, 3)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(4, 3)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(8, 3)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(12, 9)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(22, 10)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(33, 6)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Identifier, Span::new(40, 2)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_numbers() {
        let mut tokens = tokenize("1 23 -456 5.134 6.");

        assert_eq!(Some(Ok(Spanned::new(Token::Integer, Span::new(0, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Integer, Span::new(2, 2)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Integer, Span::new(5, 4)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Float, Span::new(10, 5)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Float, Span::new(16, 2)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_strings() {
        // let mut tokens = tokenize(r#""foo" "a b c" "\n""#);
        let mut tokens = tokenize(r#""\n""#);

        // assert_eq!(Some(Ok(Spanned::new(Token::String, Span::new(0, 5)))), tokens.next());
        // assert_eq!(Some(Ok(Spanned::new(Token::String, Span::new(6, 7)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::String, Span::new(14, 4)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_characters() {
        let mut tokens = tokenize(r#"'a' '8' '\n' '\''"#);

        assert_eq!(Some(Ok(Spanned::new(Token::Char, Span::new(0, 3)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Char, Span::new(4, 3)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Char, Span::new(8, 4)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Char, Span::new(13, 4)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_delimiters() {
        let mut tokens = tokenize("[()]");

        assert_eq!(Some(Ok(Spanned::new(Token::OpenBracket, Span::new(0, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::OpenParenthesis, Span::new(1, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::CloseParenthesis, Span::new(2, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::CloseBracket, Span::new(3, 1)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_symbols() {
        let mut tokens = tokenize("+-*/=!!=>>=<<=?");

        assert_eq!(Some(Ok(Spanned::new(Token::Plus, Span::new(0, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Minus, Span::new(1, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Star, Span::new(2, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Slash, Span::new(3, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Equal, Span::new(4, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Bang, Span::new(5, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::BangEqual, Span::new(6, 2)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Greater, Span::new(8, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::GreaterEqual, Span::new(9, 2)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::Less, Span::new(11, 1)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::LessEqual, Span::new(12, 2)))), tokens.next());
        assert_eq!(Some(Ok(Spanned::new(Token::QuestionMark, Span::new(14, 1)))), tokens.next());
        assert_eq!(None, tokens.next())
    }

    #[test]
    fn test_tokenize_ignores_whitespace() {
        let mut tokens = tokenize(" \r\n\t \n");

        assert_eq!(None, tokens.next())
    }
}
