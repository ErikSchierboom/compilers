use crate::lexer::{tokenize, LexError, LexResult, Token};
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
    Whitespace(String),
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

pub type ParseItemResult = ParseResult<Item>;
pub type ParseWordResult = ParseResult<Word>;
type ParseResult<T> = Result<Spanned<T>, Spanned<ParseError>>;

pub struct Parser<'a, T>
where
    T: Iterator<Item = LexResult>,
{
    tokens: Peekable<T>,
    source_code: &'a str,
    span: Span,
}

impl<'a, T> Parser<'a, T>
where
    T: Iterator<Item = LexResult>,
{
    fn new(source_code: &'a str, tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
            source_code,
            span: Span::EMPTY,
        }
    }

    fn item(&mut self) -> Option<ParseItemResult> {
        match self.next_token()? {
            Ok(token) => match token.value {
                // Token::Number => Some(self.integer()),
                // Token::OpenBracket => Some(self.array()),
                // Token::Identifier => match self.next_token_matches(&Token::Colon) {
                //     Some(_) => Some(self.binding(token)),
                //     None => Some(self.identifier()),
                // },
                // Token::Plus => Some(self.primitive(Primitive::Add)),
                // Token::Minus => Some(self.primitive(Primitive::Subtract)),
                // Token::Star => Some(self.primitive(Primitive::Multiply)),
                // Token::Slash => Some(self.primitive(Primitive::Divide)),
                // Token::Ampersand => Some(self.primitive(Primitive::And)),
                // Token::Pipe => Some(self.primitive(Primitive::Or)),
                // Token::Caret => Some(self.primitive(Primitive::Xor)),
                // Token::Bang => Some(self.primitive(Primitive::Not)),
                // Token::Underscore => Some(self.primitive(Primitive::Negate)),
                // Token::Equal => Some(self.primitive(Primitive::Equal)),
                // Token::NotEqual => Some(self.primitive(Primitive::NotEqual)),
                // Token::Greater => Some(self.primitive(Primitive::Greater)),
                // Token::GreaterEqual => Some(self.primitive(Primitive::GreaterEqual)),
                // Token::Less => Some(self.primitive(Primitive::Less)),
                // Token::LessEqual => Some(self.primitive(Primitive::LessEqual)),
                // Token::CloseBracket => Some(self.error(ParseError::Unexpected(token.value))),
                // Token::Colon => Some(self.error(ParseError::Unexpected(token.value))),
                // Token::Newline => Some(self.parse_node()),
                Token::Whitespace => Some(self.whitespace()),
                Token::Comment => Some(self.comment()),
                Token::EndOfFile => None,
            },
            Err(lex_error) => Some(self.make_error(Lex(lex_error.value))),
        }
    }

    fn words(&mut self) -> Option<ParseItemResult> {
        todo!("parse words")
    }

    fn whitespace(&mut self) -> ParseItemResult {
        let whitespace = self.lexeme(&self.span).to_string();
        self.make_item(Item::Whitespace(whitespace))
    }

    fn comment(&mut self) -> ParseItemResult {
        let comment = self.lexeme(&self.span).to_string();
        self.make_item(Item::Comment(comment))
    }

    fn integer(&mut self) -> ParseWordResult {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.make_word(Word::Integer(number))
    }

    fn identifier(&mut self) -> ParseWordResult {
        match self.lexeme(&self.span) {
            "dup" => self.primitive(Primitive::Dup),
            "drop" => self.primitive(Primitive::Drop),
            "swap" => self.primitive(Primitive::Swap),
            "over" => self.primitive(Primitive::Over),
            name => self.make_word(Word::Identifier(name.to_string())),
        }
    }

    fn primitive(&self, primitive: Primitive) -> ParseWordResult {
        self.make_word(Word::Primitive(primitive))
    }

    fn array(&mut self) -> ParseWordResult {
        let start_span = self.span.clone();
        let mut elements: Vec<Spanned<Word>> = Vec::new();

        loop {
            match self.next_token() {
                None => return self.make_error(ParseError::UnterminatedArray),
                Some(Err(lex_error)) => return self.make_error(Lex(lex_error.value)),
                Some(Ok(token)) if token.value == Token::CloseBracket => {
                    self.span = start_span.merge(&self.span);
                    return self.make_word(Word::Array(elements));
                }
                Some(Ok(token)) => match self.item(token)? {
                    Err(error) => return Some(Err(error)),
                    Ok(element) => elements.push(element),
                },
            }
        }
    }

    fn make_item(&self, item: Item) -> ParseItemResult {
        Ok(self.spanned(item))
    }

    fn make_word(&self, word: Word) -> ParseWordResult {
        Ok(self.spanned(word))
    }

    fn make_error(&mut self, error: ParseError) -> ParseItemResult {
        Err(self.spanned(error))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }

    fn next_token(&mut self) -> Option<LexResult> {
        self.next_token_if(|_| true)
    }

    fn next_token_matches(&mut self, expected: &Token) -> Option<LexResult> {
        self.next_token_if(|token| token == expected)
    }

    fn next_token_while(&mut self, func: impl Fn(&Token) -> bool) {
        while self.next_token_if(&func).is_some() {}
    }

    fn next_token_if(&mut self, func: impl Fn(&Token) -> bool) -> Option<LexResult> {
        self.tokens
            .next_if(|lex_result| match lex_result {
                Ok(token) => func(&token.value),
                _ => false,
            })
            .inspect(|lex_result| self.update_span(lex_result))
    }

    fn update_span(&mut self, token_result: &LexResult) {
        match token_result {
            Ok(token) => self.span = token.span.clone(),
            Err(error) => self.span = error.span.clone(),
        }
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }
}

impl<'a, T> Iterator for Parser<'a, T>
where
    T: Iterator<Item = LexResult>,
{
    type Item = ParseItemResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.item()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item = ParseItemResult> + '_ {
    let tokens = tokenize(source);
    Parser::new(source, tokens)
}
