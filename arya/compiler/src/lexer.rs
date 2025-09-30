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

    // Synthetic
    EndOfFile,
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
            Token::Dup => write!(f, "dup"),
            Token::Drop => write!(f, "drop"),
            Token::Swap => write!(f, "swap"),
            Token::Over => write!(f, "over"),
            Token::Reduce => write!(f, "reduce"),
            Token::Fold => write!(f, "fold"),
            Token::Both => write!(f, "both"),
            Token::EndOfFile => write!(f, "EOF"),
            Token::Reverse => write!(f, "reverse"),
            Token::Keep => write!(f, "keep"),
            Token::QuestionMark => write!(f, "?")
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
    position: i32,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    fn new(source_code: TChars) -> Self {
        Self { chars: source_code.peekable(), char: None, position: 0 }
    }

    fn lex_token(&mut self) -> LexTokenResult {
        let start = self.position;

        self.advance();

        let lex_result = match self.char {
            Some(c) => match c {
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
                    if self.advance_if_char(&'=') {
                        Ok(Token::NotEqual)
                    } else {
                        Ok(Token::Bang)
                    }
                }
                '>' => {
                    if self.advance_if_char(&'=') {
                        Ok(Token::GreaterEqual)
                    } else {
                        Ok(Token::Greater)
                    }
                }
                '<' => {
                    if self.advance_if_char(&'=') {
                        Ok(Token::LessEqual)
                    } else {
                        Ok(Token::Less)
                    }
                }
                'b' if self.advance_if_chars("oth") => Ok(Token::Both),
                'd' => {
                    if self.advance_if_chars("up") {
                        Ok(Token::Dup)
                    } else if self.advance_if_chars("rop") {
                        Ok(Token::Drop)
                    } else {
                        self.unexpected_character()
                    }
                }
                'k' if self.advance_if_chars("eep") => Ok(Token::Keep),
                'f' if self.advance_if_chars("old") => Ok(Token::Fold),
                'o' if self.advance_if_chars("ver") => Ok(Token::Over),
                'r' => {
                    if self.advance_if_chars("e") {
                        if self.advance_if_chars("duce") {
                            Ok(Token::Reduce)
                        } else if self.advance_if_chars("verse") {
                            Ok(Token::Reverse)
                        } else {
                            self.unexpected_character()
                        }
                    } else {
                        self.unexpected_character()
                    }
                }
                's' if self.advance_if_chars("wap") => Ok(Token::Swap),
                '\'' => self.lex_char(),
                '"' => self.lex_string(),
                c if c.is_ascii_digit() => self.lex_number(),
                c if c.is_ascii_whitespace() => {
                    self.advance_while(char::is_ascii_whitespace);
                    return self.lex_token();
                }
                _ => self.unexpected_character()
            },
            None => Ok(Token::EndOfFile),
        };

        match lex_result {
            Ok(token) => Ok(self.spanned(token, start)),
            Err(err) => Err(self.spanned(err, start)),
        }
    }

    fn lex_char(&mut self) -> Result<Token, LexError> {
        // TODO: support escape characters
        self.advance();
        self.lex_character()?;

        if self.advance_if_char(&'\'') {
            Ok(Token::Char)
        } else {
            Err(LexError::ExpectedCharacter('\''))
        }
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        self.advance();

        loop {
            if self.advance_if_char(&'"') {
                break;
            } else {
                self.lex_character()?;
            }
        }

        Ok(Token::String)
    }

    fn lex_number(&mut self) -> Result<Token, LexError> {
        self.advance_while(char::is_ascii_digit);
        Ok(Token::Number)
    }

    fn lex_character(&mut self) -> Result<(), LexError> {
        match self.char {
            Some('\\') => {
                self.advance();
                if let Some(c) = self.char {
                    match c {
                        'n' | 'r' | 't' | '\\' => {
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

    fn advance(&mut self) {
        self.char = self.chars.next();
        self.position += 1
    }

    fn advance_if(&mut self, predicate: impl FnOnce(&char) -> bool) -> Option<char> {
        if self.chars.peek().map(predicate)? {
            self.advance();
            self.char
        } else {
            None
        }
    }

    fn advance_if_char(&mut self, expected: &char) -> bool {
        self.advance_if(|c| c == expected).is_some()
    }

    fn advance_if_chars(&mut self, expected: &str) -> bool {
        expected.chars().all(|c| self.advance_if_char(&c))
    }

    fn advance_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while self.advance_if(&predicate).is_some() {}
    }

    fn spanned<V>(&self, value: V, start: i32) -> Spanned<V> {
        let span = Span::new(start as u32, (self.position - start) as u16);
        Spanned::new(value, span)
    }
}

impl<TChars> Iterator for Lexer<TChars>
where
    TChars: Iterator<Item=char>,
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

pub fn tokenize(source: &str) -> impl Iterator<Item=LexTokenResult> + '_ {
    Lexer::new(source.chars())
}
