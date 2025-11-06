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
    Number(i64),
    String(String),
    Identifier(String),

    // Delimiters
    OpenParenthesis,
    CloseParenthesis,

    // Symbols
    Equal,
    Comma,
    Semicolon,
    LessThan,
    Minus,
    MinusGreaterThan,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number(num) => write!(f, "{num}"),
            Token::String(str) => write!(f, "{}", str),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::Comma => write!(f, ","),
            Token::Equal => write!(f, "="),
            Token::Semicolon => write!(f, ";"),
            Token::LessThan => write!(f, "<"),
            Token::Minus => write!(f, "-"),
            Token::MinusGreaterThan => write!(f, "->"),
        }
    }
}


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
            '(' => Ok(Token::OpenParenthesis),
            ')' => Ok(Token::CloseParenthesis),
            '=' => Ok(Token::Equal),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            '-' => {
                if self.next_char_is('>') {
                    self.advance();
                    Ok(Token::MinusGreaterThan)
                } else {
                    Ok(Token::Minus)
                }
            }
            '<' => Ok(Token::LessThan),
            '\'' => self.lex_char(),
            '"' => self.lex_string(),
            '#' => {
                self.skip_comment();
                return self.lex_token();
            }
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

        while let Some(c) = self.char && c != '"' {
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
            Ok(Token::Int)
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

    fn skip_comment(&mut self) {
        self.advance();
        self.advance_while_chars_match(|&c| c != '\n')
    }
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source.chars());
    lexer.lex()
}
