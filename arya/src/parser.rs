use crate::scanner::{scan, ScanError, Token};
use crate::source::{Location, Source, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ParseError {
    Scan(ScanError)
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Scan(scan_error) => write!(f, "{}", scan_error)
        }
    }
}

impl Error for ParseError {}

#[derive(Debug)]
pub enum Node {
    Number(i64),
    Character(char),
    String(String),
    Identifier(String),
    Array(Vec<Node>),
    Error(ParseError)
}

type ParseNodeResult = Result<Spanned<Node>, Spanned<ParseError>>;
type ParseResult = Result<Vec<Spanned<Node>>, Vec<Spanned<ParseError>>>;

pub struct Parser<'a> {
    tokens: Vec<Spanned<Token>>,
    start: Location,
    source: &'a Source
}

impl<'a> Parser<'a>  {
    pub fn new(source: &'a Source, tokens: Vec<Spanned<Token>>) -> Self {
        let start = Location::new();
        Parser { tokens, start, source }
    }

    pub fn parse(&mut self) -> ParseResult {
        let mut tokens: Vec<Spanned<Node>> = Vec::new();
        let mut errors: Vec<Spanned<ParseError>> = Vec::new();

        // while let Some(scan_token_result) = self.scan_token() {
        //     match scan_token_result {
        //         Ok(token) => tokens.push(token),
        //         Err(error) => errors.push(error)
        //     }
        // }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(tokens)
        }
    }
}


pub fn parse(source: &Source) -> ParseResult {
    match scan(source) {
        Ok(tokens) => {
            let mut parser = Parser::new(source, tokens);
            parser.parse()
        }
        Err(errors) =>
            Err(errors.into_iter()
                    .map(|error|error.map_value(ParseError::Scan))
                    .collect())
    }
}

