use crate::location::{Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Clone, Debug)]
pub enum LexError {
    UnexpectedCharacter(char),
    ExpectedCharacter(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedCharacter(c) => write!(f, "Unexpected character '{c}'"),
            LexError::ExpectedCharacter(c) => write!(f, "Expected character '{c}'"),
        }
    }
}

impl Error for LexError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // TODO: Char
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

pub type LexTokenResult<T = Spanned<Token>, E = Spanned<LexError>> = Result<T, E>;

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
        Self { chars: source_code.peekable(), char: None, position: -1 }
    }

    fn lex_token(&mut self) -> LexTokenResult {
        self.advance();
        self.skip_whitespace();

        let start = self.position;

        let token = match self.char {
            None => Token::EndOfFile,
            Some(c) => match c {
                '[' => Token::OpenBracket,
                ']' => Token::CloseBracket,
                '(' => Token::OpenParenthesis,
                ')' => Token::CloseParenthesis,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '^' => Token::Caret,
                '&' => Token::Ampersand,
                '|' => Token::Pipe,
                '_' => Token::Underscore,
                '?' => Token::QuestionMark,
                '=' => Token::Equal,
                '!' => {
                    if self.advance_if_char(&'=') {
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                '>' => {
                    if self.advance_if_char(&'=') {
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                }
                '<' => {
                    if self.advance_if_char(&'=') {
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                'b' if self.advance_if_chars("oth") => Token::Both,
                'd' => {
                    if self.advance_if_chars("up") {
                        Token::Dup
                    } else if self.advance_if_chars("rop") {
                        Token::Drop
                    } else {
                        return self.lex_token();
                    }
                }
                'k' if self.advance_if_chars("eep") => Token::Keep,
                'f' if self.advance_if_chars("old") => Token::Fold,
                'o' if self.advance_if_chars("ver") => Token::Over,
                'r' => {
                    if self.advance_if_chars("e") {
                        if self.advance_if_chars("duce") {
                            Token::Reduce
                        } else if self.advance_if_chars("verse") {
                            Token::Reverse
                        } else {
                            return self.lex_token();
                        }
                    } else {
                        return self.lex_token();
                    }
                }
                's' if self.advance_if_chars("wap") => Token::Swap,
                '"' => {
                    self.advance();
                    // TODO: support escape characters
                    self.advance_while(|&c| c != '"');
                    if self.advance_if_char(&'"') {
                        Token::String
                    } else {
                        return Err(self.spanned(LexError::ExpectedCharacter('"'), start));
                    }
                }
                c if c.is_ascii_digit() => {
                    self.advance_while(char::is_ascii_digit);
                    Token::Number
                }
                c => return Err(self.spanned(LexError::UnexpectedCharacter(c), start)),
            },
        };


        Ok(self.spanned(token, start))
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace)
    }

    fn advance(&mut self) {
        self.char = self.chars.next();
        self.position += 1
    }

    fn advance_if(&mut self, predicate: impl FnOnce(&char) -> bool) -> Option<char> {
        self.char.filter(predicate).inspect(|_| self.advance())
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
