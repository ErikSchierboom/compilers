use crate::lexer::{tokenize, LexError, ParseTokenResult, Token};
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
pub enum Word {
    Integer(i64),
    Identifier(String),
    Primitive(Primitive),
    Array(Vec<Spanned<Word>>),
    Binding(String, Vec<Spanned<Word>>),
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

pub type ParseNodeResult = Result<Spanned<Word>, Spanned<ParseError>>;

pub struct Parser<'a, T>
where
    T: Iterator<Item = ParseTokenResult>,
{
    tokens: Peekable<T>,
    source_code: &'a str,
    span: Span,
}

impl<'a, T> Parser<'a, T>
where
    T: Iterator<Item = ParseTokenResult>,
{
    fn new(source_code: &'a str, tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
            source_code,
            span: Span::EMPTY,
        }
    }

    fn parse_node(&mut self) -> Option<ParseNodeResult> {
        match self.next()? {
            Ok(token) => self.parse_node_from_token(token),
            Err(lex_error) => self.error(Lex(lex_error.value)),
        }
    }

    fn parse_node_from_token(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
        match spanned_token.value {
            Token::Number => self.integer(),
            Token::OpenBracket => self.array(),
            Token::Identifier => match self.next_token_matches(&Token::Colon) {
                Some(_) => self.binding(spanned_token),
                None => self.identifier(),
            },
            Token::Plus => self.primitive(Primitive::Add),
            Token::Minus => self.primitive(Primitive::Subtract),
            Token::Star => self.primitive(Primitive::Multiply),
            Token::Slash => self.primitive(Primitive::Divide),
            Token::Ampersand => self.primitive(Primitive::And),
            Token::Pipe => self.primitive(Primitive::Or),
            Token::Caret => self.primitive(Primitive::Xor),
            Token::Bang => self.primitive(Primitive::Not),
            Token::Underscore => self.primitive(Primitive::Negate),
            Token::Equal => self.primitive(Primitive::Equal),
            Token::NotEqual => self.primitive(Primitive::NotEqual),
            Token::Greater => self.primitive(Primitive::Greater),
            Token::GreaterEqual => self.primitive(Primitive::GreaterEqual),
            Token::Less => self.primitive(Primitive::Less),
            Token::LessEqual => self.primitive(Primitive::LessEqual),
            Token::CloseBracket => self.error(ParseError::Unexpected(spanned_token.value)),
            Token::Colon => self.error(ParseError::Unexpected(spanned_token.value)),
            Token::Newline => self.parse_node(),
            Token::Whitespace => self.whitepace(),
            Token::Comment => self.comment(),
            Token::EndOfFile => None,
        }
    }

    fn integer(&mut self) -> Option<ParseNodeResult> {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.node(Word::Integer(number))
    }

    fn identifier(&mut self) -> Option<ParseNodeResult> {
        match self.lexeme(&self.span) {
            "dup" => self.primitive(Primitive::Dup),
            "drop" => self.primitive(Primitive::Drop),
            "swap" => self.primitive(Primitive::Swap),
            "over" => self.primitive(Primitive::Over),
            name => self.node(Word::Identifier(name.to_string())),
        }
    }

    fn primitive(&self, primitive: Primitive) -> Option<ParseNodeResult> {
        self.node(Word::Primitive(primitive))
    }

    fn array(&mut self) -> Option<ParseNodeResult> {
        let start_span = self.span.clone();
        let mut elements: Vec<Spanned<Word>> = Vec::new();

        loop {
            match self.next() {
                None => return self.error(ParseError::UnterminatedArray),
                Some(Err(lex_error)) => return self.error(Lex(lex_error.value)),
                Some(Ok(token)) if token.value == Token::CloseBracket => {
                    self.span = start_span.merge(&self.span);
                    return self.node(Word::Array(elements));
                }
                Some(Ok(token)) => match self.parse_node_from_token(token)? {
                    Err(error) => return Some(Err(error)),
                    Ok(element) => elements.push(element),
                },
            }
        }
    }

    fn binding(&mut self, identifier: Spanned<Token>) -> Option<ParseNodeResult> {
        let mut body: Vec<Spanned<Word>> = Vec::new();

        loop {
            match self.next() {
                None => {
                    self.span = identifier.span.merge(&self.span);
                    return self.node(Word::Binding(
                        self.lexeme(&identifier.span).to_string(),
                        body,
                    ));
                }
                Some(Ok(token)) if token.value == Token::Newline => {
                    // TODO: get identifier before updating span, then the lexeme method can use the
                    // current span to fetch the lexeme
                    self.span = identifier.span.merge(&self.span);
                    return self.node(Word::Binding(
                        self.lexeme(&identifier.span).to_string(),
                        body,
                    ));
                }
                Some(Err(lex_error)) => return self.error(Lex(lex_error.value)),
                Some(Ok(token)) => match self.parse_node_from_token(token)? {
                    Err(error) => return Some(Err(error)),
                    Ok(element) => body.push(element),
                },
            }
        }
    }

    fn whitepace(&self) -> Option<ParseNodeResult> {
        let whitespace = self.lexeme(&self.span).to_string();
        self.node(Word::Whitespace(whitespace))
    }

    fn comment(&self) -> Option<ParseNodeResult> {
        let comment = self.lexeme(&self.span).to_string();
        self.node(Word::Comment(comment))
    }

    fn node(&self, node: Word) -> Option<ParseNodeResult> {
        Some(Ok(self.spanned(node)))
    }

    fn error(&self, error: ParseError) -> Option<ParseNodeResult> {
        Some(Err(self.spanned(error)))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }

    fn next(&mut self) -> Option<ParseTokenResult> {
        self.next_if(|_| true)
    }

    fn next_token_matches(&mut self, token: &Token) -> Option<ParseTokenResult> {
        self.tokens.next_if(|parse_result| match parse_result {
            Ok(spanned_token) => &spanned_token.value == token,
            _ => false,
        })
    }

    fn next_if(&mut self, func: impl Fn(&ParseTokenResult) -> bool) -> Option<ParseTokenResult> {
        self.tokens
            .next_if(func)
            .inspect(|token_result| self.update_span(token_result))
    }

    fn update_span(&mut self, token_result: &ParseTokenResult) {
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
    T: Iterator<Item = ParseTokenResult>,
{
    type Item = ParseNodeResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_node()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item = ParseNodeResult> + '_ {
    let tokens = tokenize(source);
    Parser::new(source, tokens)
}
