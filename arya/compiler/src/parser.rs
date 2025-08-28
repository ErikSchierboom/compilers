use crate::lexer::{tokenize, LexError, LexTokenResult, Token};
use crate::location::{Span, Spanned};
use crate::parser::ParseError::Lex;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Unexpected(Token),
    UnterminatedArray,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::Unexpected(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::UnterminatedArray => write!(f, "Unterminated array"),
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug)]
pub enum Item {
    Words(Vec<Spanned<Word>>),
    Binding(Binding),
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub identifier: Spanned<Token>,
    pub colon: Spanned<Token>,
    pub words: Vec<Spanned<Word>>,
}

#[derive(Clone, Debug)]
pub enum Word {
    Integer(i64),
    Identifier(String),
    Primitive(Primitive),
    Array(Vec<Spanned<Word>>),
    Comment(String),
}

#[derive(Clone, Debug)]
pub enum Primitive {
    Add,
    Subtract,
    Multiply,
    Divide,
    Xor,
    And,
    Or,
    Not,
    Negate,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Dup,
    Drop,
    Swap,
    Over,
}

pub type LexLineResult = Result<Vec<Spanned<Token>>, Spanned<LexError>>;

pub struct LineIterator<T>
where
    T: Iterator<Item = LexTokenResult>,
{
    tokens: Peekable<T>,
}

impl<T> LineIterator<T>
where
    T: Iterator<Item = LexTokenResult>,
{
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }
}

impl<T> Iterator for LineIterator<T>
where
    T: Iterator<Item = LexTokenResult>,
{
    type Item = LexLineResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_none() {
            return None;
        }

        let mut line_lex_results: Vec<LexTokenResult> = Vec::new();
        while let Some(lex_result) = self.tokens.next() {
            if lex_result
                .as_ref()
                .is_ok_and(|token| token.value == Token::Newline)
            {
                break;
            }

            line_lex_results.push(lex_result)
        }

        Some(line_lex_results.into_iter().collect())
    }
}

pub type ParseItemResult = ParseResult<Spanned<Item>>;
pub type ParseWordResult = ParseResult<Spanned<Word>>;
type ParseResult<T> = Result<T, Spanned<ParseError>>;

pub struct Parser<'a, TLines>
where
    TLines: Iterator<Item = LexLineResult>,
{
    source_code: &'a str,
    lines: TLines,
    span: Span,
}

impl<'a, TLines> Parser<'a, TLines>
where
    TLines: Iterator<Item = LexLineResult>,
{
    fn new(source_code: &'a str, lines: TLines) -> Self {
        Parser {
            source_code,
            lines,
            span: Span::EMPTY,
        }
    }

    fn parse_item(&mut self) -> Option<ParseItemResult> {
        self.parse_binding().or_else(|| self.parse_words())
    }

    fn parse_binding(&mut self) -> Option<ParseItemResult> {
        todo!()
    }

    fn parse_words(&mut self) -> Option<ParseItemResult> {
        todo!("parse words")
    }

    fn parse_word(&mut self) -> Option<ParseWordResult> {
        match self.next_token()? {
            Err(lex_error) => Some(self.make_error(Lex(lex_error.value))),
            Ok(token) => match token.value {
                Token::Identifier => Some(self.parse_identifier()),
                Token::Number => Some(self.parse_integer()),
                Token::OpenBracket => Some(self.parse_array()),
                Token::Plus => Some(self.parse_primitive(Primitive::Add)),
                Token::Minus => Some(self.parse_primitive(Primitive::Subtract)),
                Token::Star => Some(self.parse_primitive(Primitive::Multiply)),
                Token::Slash => Some(self.parse_primitive(Primitive::Divide)),
                Token::Ampersand => Some(self.parse_primitive(Primitive::And)),
                Token::Pipe => Some(self.parse_primitive(Primitive::Or)),
                Token::Caret => Some(self.parse_primitive(Primitive::Xor)),
                Token::Bang => Some(self.parse_primitive(Primitive::Not)),
                Token::Underscore => Some(self.parse_primitive(Primitive::Negate)),
                Token::Equal => Some(self.parse_primitive(Primitive::Equal)),
                Token::NotEqual => Some(self.parse_primitive(Primitive::NotEqual)),
                Token::Greater => Some(self.parse_primitive(Primitive::Greater)),
                Token::GreaterEqual => Some(self.parse_primitive(Primitive::GreaterEqual)),
                Token::Less => Some(self.parse_primitive(Primitive::Less)),
                Token::LessEqual => Some(self.parse_primitive(Primitive::LessEqual)),
                Token::CloseBracket => Some(self.make_error(ParseError::Unexpected(token.value))),
                Token::Colon => Some(self.make_error(ParseError::Unexpected(token.value))),
                Token::Comment => Some(self.parse_comment()),
                Token::Newline | Token::EndOfFile => None,
            },
        }
    }

    fn parse_comment(&mut self) -> ParseWordResult {
        let comment = self.lexeme(&self.span).to_string();
        self.make_word(Word::Comment(comment))
    }

    fn parse_integer(&mut self) -> ParseWordResult {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.make_word(Word::Integer(number))
    }

    fn parse_identifier(&mut self) -> ParseWordResult {
        match self.lexeme(&self.span) {
            "dup" => self.parse_primitive(Primitive::Dup),
            "drop" => self.parse_primitive(Primitive::Drop),
            "swap" => self.parse_primitive(Primitive::Swap),
            "over" => self.parse_primitive(Primitive::Over),
            name => self.make_word(Word::Identifier(name.to_string())),
        }
    }

    fn parse_primitive(&self, primitive: Primitive) -> ParseWordResult {
        self.make_word(Word::Primitive(primitive))
    }

    fn parse_array(&mut self) -> ParseWordResult {
        todo!()
        // let start_span = self.span.clone();
        // let mut elements: Vec<Spanned<Word>> = Vec::new();
        //
        // loop {
        //     match self.next_token() {
        //         None => return self.make_error(ParseError::UnterminatedArray),
        //         Some(Err(lex_error)) => return self.make_error(Lex(lex_error.value)),
        //         Some(Ok(token)) if token.value == Token::CloseBracket => {
        //             self.span = start_span.merge(&self.span);
        //             return self.make_word(Word::Array(elements));
        //         }
        //         Some(Ok(token)) => match self.item(token)? {
        //             Err(error) => return Some(Err(error)),
        //             Ok(element) => elements.push(element),
        //         },
        //     }
        // }
    }

    fn make_item(&self, item: Item) -> ParseItemResult {
        Ok(self.spanned(item))
    }

    fn make_word(&self, word: Word) -> ParseWordResult {
        Ok(self.spanned(word))
    }

    fn make_error<V>(&mut self, error: ParseError) -> ParseResult<V> {
        Err(self.spanned(error))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }

    fn next_token(&mut self) -> Option<LexTokenResult> {
        self.next_token_if(|_| true)
    }

    fn next_token_matches(&mut self, expected: &Token) -> Option<LexTokenResult> {
        self.next_token_if(|token| token == expected)
    }

    fn next_token_while(&mut self, func: impl Fn(&Token) -> bool) {
        while self.next_token_if(&func).is_some() {}
    }

    fn next_token_if(&mut self, func: impl Fn(&Token) -> bool) -> Option<LexTokenResult> {
        todo!()
        // self.tokens
        //     .next_if(|lex_result| match lex_result {
        //         Ok(token) => func(&token.value),
        //         _ => false,
        //     })
        //     .inspect(|lex_result| self.update_span(lex_result))
    }

    fn update_span(&mut self, token_result: &LexTokenResult) {
        match token_result {
            Err(error) => self.span = error.span.clone(),
            Ok(token) => self.span = token.span.clone(),
        }
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }
}

impl<'a, TLines> Iterator for Parser<'a, TLines>
where
    TLines: Iterator<Item = LexLineResult>,
{
    type Item = ParseItemResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_item()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item = ParseItemResult> + '_ {
    let tokens = tokenize(source);
    let lines = LineIterator::new(tokens);
    Parser::new(source, lines)
}
