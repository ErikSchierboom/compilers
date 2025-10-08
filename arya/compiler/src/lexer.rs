use crate::location::{Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Clone, Debug)]
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
    Char,
    String,
    Number,

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
    Caret,
    Ampersand,
    Pipe,
    Bang,
    Underscore,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    QuestionMark,

    // Words
    Dup,
    Drop,
    Swap,
    Over,
    Reduce,
    Fold,
    Both,
    Keep,
    Reverse,
    Max,
    Min,
    Range,
    Partition,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number => write!(f, "number"),
            Token::String => write!(f, "string"),
            Token::Char => write!(f, "char"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Caret => write!(f, "^"),
            Token::Ampersand => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Bang => write!(f, "!"),
            Token::Underscore => write!(f, "_"),
            Token::Equal => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::QuestionMark => write!(f, "?"),
            Token::Dup => write!(f, "dup"),
            Token::Drop => write!(f, "drop"),
            Token::Swap => write!(f, "swap"),
            Token::Over => write!(f, "over"),
            Token::Reduce => write!(f, "reduce"),
            Token::Fold => write!(f, "fold"),
            Token::Both => write!(f, "both"),
            Token::Reverse => write!(f, "reverse"),
            Token::Keep => write!(f, "keep"),
            Token::Max => write!(f, "max"),
            Token::Min => write!(f, "min"),
            Token::Range => write!(f, "range"),
            Token::Partition => write!(f, "partition"),
        }
    }
}

pub type LexTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

struct Lexer<TChars>
where
    TChars: Iterator<Item=(u32, char)>,
{
    chars: Peekable<TChars>,
    char: Option<char>,
    position: u32,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=(u32, char)>,
{
    fn new(source_code: TChars) -> Self {
        Self { chars: source_code.peekable(), char: None, position: 0 }
    }

    fn lex_token(&mut self) -> Option<LexTokenResult> {
        self.skip_whitespace();

        let (start, c) = self.next_char()?;

        let result = match c {
            '[' => Ok(Token::OpenBracket),
            ']' => Ok(Token::CloseBracket),
            '(' => Ok(Token::OpenParenthesis),
            ')' => Ok(Token::CloseParenthesis),
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '*' => Ok(Token::Star),
            '/' => Ok(Token::Slash),
            '^' => Ok(Token::Caret),
            '&' => Ok(Token::Ampersand),
            '|' => Ok(Token::Pipe),
            '_' => Ok(Token::Underscore),
            '?' => Ok(Token::QuestionMark),
            '=' => Ok(Token::Equal),
            '!' => {
                if self.next_if_char_is('=') {
                    Ok(Token::NotEqual)
                } else {
                    Ok(Token::Bang)
                }
            }
            '>' => {
                if self.next_if_char_is('=') {
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            }
            '<' => {
                if self.next_if_char_is('=') {
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            }
            'b' if self.next_if_followed_by("oth") => Ok(Token::Both),
            'd' => {
                if self.next_if_followed_by("up") {
                    Ok(Token::Dup)
                } else if self.next_if_followed_by("rop") {
                    Ok(Token::Drop)
                } else {
                    self.unexpected_character()
                }
            }
            'k' if self.next_if_followed_by("eep") => Ok(Token::Keep),
            'f' if self.next_if_followed_by("old") => Ok(Token::Fold),
            'm' => {
                if self.next_if_followed_by("ax") {
                    Ok(Token::Max)
                } else if self.next_if_followed_by("in") {
                    Ok(Token::Min)
                } else {
                    self.unexpected_character()
                }
            }
            'o' if self.next_if_followed_by("ver") => Ok(Token::Over),
            'p' if self.next_if_followed_by("artition") => Ok(Token::Partition),
            'r' => {
                if self.next_if_followed_by("ange") {
                    Ok(Token::Range)
                } else if self.next_if_followed_by("e") {
                    if self.next_if_followed_by("duce") {
                        Ok(Token::Reduce)
                    } else if self.next_if_followed_by("verse") {
                        Ok(Token::Reverse)
                    } else {
                        self.unexpected_character()
                    }
                } else {
                    self.unexpected_character()
                }
            }
            's' if self.next_if_followed_by("wap") => Ok(Token::Swap),
            '\'' => self.lex_char(),
            '"' => self.lex_string(),
            c if c.is_ascii_digit() => self.lex_number(),
            _ => self.unexpected_character()
        };

        match result {
            Ok(token) => Some(Ok(self.spanned(token, start))),
            Err(err) => Some(Err(self.spanned(err, start))),
        }
    }

    fn lex_char(&mut self) -> Result<Token, LexError> {
        self.next_char();
        self.lex_character()?;

        if self.next_if_char_is('\'') {
            Ok(Token::Char)
        } else {
            Err(LexError::ExpectedCharacter('\''))
        }
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        self.next_char();

        loop {
            if self.next_if_char_is('"') {
                break;
            } else {
                self.lex_character()?;
            }
        }

        Ok(Token::String)
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        self.next_while_chars_match(char::is_ascii_digit);
        Ok(Token::Number)
    }

    fn lex_character(&mut self) -> Result<(), LexError> {
        match self.char {
            Some('\\') => {
                self.next_char();
                if let Some(c) = self.char {
                    match c {
                        'n' | 'r' | 't' | '\\' => {
                            self.next_char();
                            Ok(())
                        }
                        _ => Err(LexError::InvalidEscape(c))
                    }
                } else {
                    Err(LexError::UnexpectedEndOfFile)
                }
            }
            Some(_) => {
                self.next_char();
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

    fn next_char(&mut self) -> Option<(u32, char)> {
        let (position, char) = self.chars.next()?;
        self.position = position;
        self.char = Some(char);
        Some((position, char))
    }

    fn next_if_char_matches(&mut self, predicate: impl FnOnce(&char) -> bool) -> Option<char> {
        if self.chars.peek().map(|(_, c)| predicate(c))? {
            self.next_char();
            self.char
        } else {
            None
        }
    }

    fn next_if_char_is(&mut self, expected: char) -> bool {
        self.next_if_char_matches(|c| *c == expected).is_some()
    }

    fn next_if_followed_by(&mut self, expected: &str) -> bool {
        expected.chars().all(|c| self.next_if_char_is(c))
    }

    fn next_while_chars_match(&mut self, predicate: impl Fn(&char) -> bool) {
        while self.next_if_char_matches(&predicate).is_some() {}
    }

    fn skip_whitespace(&mut self) {
        self.next_while_chars_match(char::is_ascii_whitespace)
    }

    fn spanned<V>(&self, value: V, start: u32) -> Spanned<V> {
        let span = Span::new(start, (self.position - start) as u16 + 1);
        Spanned::new(value, span)
    }
}

impl<TChars> Iterator for Lexer<TChars>
where
    TChars: Iterator<Item=(u32, char)>,
{
    type Item = LexTokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

pub fn tokenize(source: &str) -> impl Iterator<Item=LexTokenResult> + '_ {
    Lexer::new(source.char_indices().map(|(i, c)| (i as u32, c)))
}
