use crate::scanner::{scan, ScanError, Token};
use crate::source::{Source, Span, Spanned};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Clone, Debug)]
pub enum ParseError {
    Scan(ScanError),
    Unexpected(Token),
    Expected(Token)
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Scan(scan_error) => write!(f, "{}", scan_error),
            ParseError::Unexpected(token) => write!(f, "Unexpected token: {token}"),
            ParseError::Expected(token) => write!(f, "Expected token: {token}"),
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
    Array(Vec<Node>)
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Number(n) => write!(f, "Number: {n}"),
            Node::Character(c) => write!(f, "Character: {c}"),
            Node::String(string) => write!(f, "String: {string}"),
            Node::Identifier(identifier) => write!(f, "Identifier: {identifier}"),
            Node::Array(elements) => write!(f, "Array: {:?}", elements)
        }
    }
}

struct TokenWindow {
    tokens: Peekable<std::vec::IntoIter<Spanned<Token>>>
}

impl TokenWindow {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        let tokens = tokens.into_iter().peekable();
        TokenWindow { tokens }
    }

    pub fn peek(&mut self) -> Option<&Spanned<Token>> {
        self.tokens.peek()
    }

    pub fn advance(&mut self) -> Option<Spanned<Token>> {
        self.advance_if(|_| true)
    }

    pub fn advance_if(&mut self, func: impl Fn(&Token) -> bool) -> Option<Spanned<Token>> {
        self.tokens.next_if(|spanned| func(&spanned.value))
    }
}

type ParseNodeResult = Result<Spanned<Node>, Spanned<ParseError>>;
type ParseResult = Result<Vec<Spanned<Node>>, Vec<Spanned<ParseError>>>;

pub struct Parser<'a> {
    tokens: TokenWindow,
    source_code: &'a str
}

impl<'a> Parser<'a>  {
    pub fn new(source: &'a Source, tokens: Vec<Spanned<Token>>) -> Self {
        let tokens = TokenWindow::new(tokens);
        let source_code = source.source_code();
        Parser { tokens, source_code }
    }

    pub fn parse(&mut self) -> ParseResult {
        let mut nodes: Vec<Spanned<Node>> = Vec::new();
        let mut errors: Vec<Spanned<ParseError>> = Vec::new();

        while let Some(parse_result) = self.parse_node() {
            match parse_result {
                Ok(node) => nodes.push(node),
                Err(error) => errors.push(error)
            }
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(nodes)
        }
    }

    fn parse_node(&mut self) -> Option<ParseNodeResult> {
        let spanned_token = self.tokens.advance()?;
        match spanned_token.value {
            Token::Number => self.number(spanned_token),
            Token::Character => self.character(spanned_token),
            Token::String => self.string(spanned_token),
            Token::Identifier => self.identifier(spanned_token),
            Token::OpenBracket => self.array(spanned_token),
            Token::CloseBracket => Some(Err(spanned_token.with_value(ParseError::Unexpected(Token::CloseBracket))))
        }
    }

    fn array(&mut self, open_bracket_token: Spanned<Token>) -> Option<ParseNodeResult> {
        let mut elements: Vec<Node> = Vec::new();
        let mut last_span = open_bracket_token.span.clone();

        loop {
            match self.tokens.peek() {
                None => return Some(Err(Spanned::new(ParseError::Expected(Token::CloseBracket), last_span))),
                Some(Spanned { value: Token::CloseBracket, span}) => {
                    last_span = span.clone();
                    break
                }
                Some(spanned_token) => {
                    last_span = spanned_token.span.clone();

                    match self.parse_node().unwrap() {
                        Ok(element) => elements.push(element.value),
                        Err(error) => return Some(Err(error))
                    }
                }
            }
        }

        self.tokens.advance();

        Some(Ok(Spanned::new(Node::Array(elements), open_bracket_token.span + last_span)))
    }

    fn identifier(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
        let str = self.lexeme(&spanned_token.span).to_string();
        Some(Ok(spanned_token.with_value(Node::Identifier(str))))
    }

    fn string(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
        let str = self.lexeme(&spanned_token.span).to_string();
        Some(Ok(spanned_token.with_value(Node::String(str))))
    }

    fn character(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
        // TODO: check if we need error handling
        let c = self.lexeme(&spanned_token.span).chars().next().unwrap();
        Some(Ok(spanned_token.with_value(Node::Character(c))))
    }

    fn number(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
        // TODO: check if we need error handling
        let number = i64::from_str(self.lexeme(&spanned_token.span)).unwrap();
        Some(Ok(spanned_token.with_value(Node::Number(number))))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        let begin = span.begin.position as usize;
        let end = span.end.position as usize;
        &self.source_code[begin..end]
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
                    .map(|error|error.map_value(|e|ParseError::Scan(e.clone())))
                    .collect())
    }
}

