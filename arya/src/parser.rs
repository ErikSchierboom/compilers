use crate::scanner::{scan, ScanError, Token};
use crate::source::{Location, Source, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Scan(ScanError)
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Scan(scan_error) => write!(f, "{scan_error}")
        }
    }
}

impl Error for ParseError {}

pub enum Node {
    Number(i64),
    Character(char),
    String(String),
    Identifier(String),
    Array(Vec<Node>)
}

pub struct Parser<'a, I> where I: Iterator<Item = Spanned<Token>> {
    tokens: Peekable<I>,
    start: Location,
    source: &'a Source
}

impl<'a, I> Parser<'a, I> where I: Iterator<Item = Spanned<Token>> {
    pub fn new(tokens: I, source: &'a Source) -> Self {
        let start = Location::new();
        Parser { tokens: tokens.peekable(), start, source }
    }
}

impl<'a, I> Iterator for Parser<'a, I> where I: Iterator<Item = Spanned<Token>> {
    type Item = Spanned<Node>;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

pub fn parse<I>(source: &Source) -> Vec<Spanned<Node>> {
    let tokens = scan::<Spanned<Node>>(source);
    Parser::new(tokens, source).collect()
}
