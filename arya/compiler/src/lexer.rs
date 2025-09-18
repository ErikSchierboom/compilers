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

    // Words
    Dup,
    Drop,
    Swap,
    Over,
    Reduce,
    Fold,
    Bracket,
    Both,

    // Synthetic
    EndOfFile,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Number => write!(f, "number"),
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
            Token::Bracket => write!(f, "bracket"),
            Token::Both => write!(f, "both"),
            Token::EndOfFile => write!(f, "EOF"),
        }
    }
}

pub type LexTokenResult = Result<Spanned<Token>, Spanned<LexError>>;

struct Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    chars: Peekable<TChars>,
    span: Span,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    fn new(source_code: TChars) -> Self {
        Self { chars: source_code.peekable(), span: Span::EMPTY }
    }

    fn lex_token(&mut self) -> LexTokenResult {
        self.skip_whitespace();
        self.update_position();

        // TODO: inline make_token and assign match result token into variable
        match self.next_char() {
            None => self.make_token(Token::EndOfFile),
            Some(c) => match c {
                '[' => self.make_token(Token::OpenBracket),
                ']' => self.make_token(Token::CloseBracket),
                '(' => self.make_token(Token::OpenParenthesis),
                ')' => self.make_token(Token::CloseParenthesis),
                '+' => self.make_token(Token::Plus),
                '-' => self.make_token(Token::Minus),
                '*' => self.make_token(Token::Star),
                '/' => self.make_token(Token::Slash),
                '^' => self.make_token(Token::Caret),
                '&' => self.make_token(Token::Ampersand),
                '|' => self.make_token(Token::Pipe),
                '_' => self.make_token(Token::Underscore),
                '=' => self.make_token(Token::Equal),
                '!' => {
                    if self.next_if_char_is(&'=') {
                        self.make_token(Token::NotEqual)
                    } else {
                        self.make_token(Token::Bang)
                    }
                }
                '>' => {
                    if self.next_if_char_is(&'=') {
                        self.make_token(Token::GreaterEqual)
                    } else {
                        self.make_token(Token::Greater)
                    }
                }
                '<' => {
                    if self.next_if_char_is(&'=') {
                        self.make_token(Token::LessEqual)
                    } else {
                        self.make_token(Token::Less)
                    }
                }
                'd' => {
                    if self.next_if_chars_are("up") {
                        self.make_token(Token::Dup)
                    } else if self.next_if_chars_are("rop") {
                        self.make_token(Token::Drop)
                    } else {
                        self.make_error(LexError::UnexpectedCharacter(c))
                    }
                }
                's' if self.next_if_chars_are("wap") => self.make_token(Token::Swap),
                'o' if self.next_if_chars_are("ver") => self.make_token(Token::Over),
                'r' if self.next_if_chars_are("educe") => self.make_token(Token::Reduce),
                c if c.is_ascii_digit() => self.lex_number(),
                c => self.make_error(LexError::UnexpectedCharacter(c)),
            },
        }
    }

    fn update_position(&mut self) {
        self.span.position += self.span.length as u32;
        self.span.length = 0;
    }

    fn lex_number(&mut self) -> LexTokenResult {
        self.skip_while(char::is_ascii_digit);
        self.make_token(Token::Number)
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(char::is_ascii_whitespace)
    }

    fn next_char(&mut self) -> Option<char> {
        self.next_char_if(|_| true)
    }

    fn next_char_if(&mut self, predicate: impl Fn(&char) -> bool) -> Option<char> {
        self.chars.next_if(predicate).inspect(|_| self.span.length += 1)
    }

    fn next_if_char_is(&mut self, expected: &char) -> bool {
        self.next_char_if(|c| c == expected).is_some()
    }

    fn next_if_chars_are(&mut self, expected: &str) -> bool {
        for expected_char in expected.chars() {
            if self.next_char_if(|&c| c == expected_char).is_none() {
                return false;
            }
        }

        true
    }

    fn skip_while(&mut self, predicate: impl Fn(&char) -> bool) {
        while self.next_char_if(&predicate).is_some() {}
    }

    fn make_token(&mut self, token: Token) -> LexTokenResult {
        Ok(self.spanned(token))
    }

    fn make_error(&mut self, error: LexError) -> LexTokenResult {
        Err(self.spanned(error))
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
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
