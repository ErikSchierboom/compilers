use crate::lex::{tokenize, LexErrorKind, Span, TokenKind, TokenResult};
use crate::parse::NodeValue::{Integer, Operator};
use crate::parse::ParseErrorKind::Lex;
use ecow::EcoString;
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexErrorKind),
    Unexpected(TokenKind),
    UnterminatedArray,
}

pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug)]
pub enum NodeValue {
    Integer(i64),
    Character(char),
    String(EcoString),
    Operator(Op),
    Array(Vec<Node>)
}

#[derive(Debug)]
pub struct Node {
    pub value: NodeValue,
    pub span: Span
}

impl Node {
    pub fn new(value: NodeValue, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Debug)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide
}

struct TokenWindow<T: Iterator<Item =TokenResult>> {
    tokens: Peekable<T>
}

impl<T> TokenWindow<T> where T : Iterator<Item =TokenResult> {
    pub fn new(tokens: T) -> Self {
        TokenWindow { tokens: tokens.peekable() }
    }

    pub fn peek(&mut self) -> Option<&TokenResult> {
        self.tokens.peek()
    }

    pub fn advance(&mut self) -> Option<TokenResult> {
        self.advance_if(|_| true)
    }

    pub fn advance_if(&mut self, func: impl Fn(&TokenKind) -> bool) -> Option<TokenResult> {
        self.tokens.next_if(|lex_result| {
            match lex_result {
                Ok(token) => func(&token.kind),
                _ => false
            }
        })
    }
}

type ParseNodeResult = Result<Node, ParseError>;
pub type ParseResult = Result<Vec<Node>, ParseError>;

pub struct Parser<'a, T> where T : Iterator<Item = TokenResult> {
    tokens: TokenWindow<T>,
    source_code: &'a str,
    span: Span
}

impl<'a, T> Parser<'a, T> where T : Iterator<Item = TokenResult> {
    pub fn new(source: &'a str, tokens: T) -> Self {
        Parser { tokens: TokenWindow::new(tokens), source_code: source, span: Span::new(0, 0) }
    }

    pub fn parse(&mut self) -> ParseResult {
        let mut nodes: Vec<Node> = Vec::new();
        
        while let Some(parse_node_result) = self.parse_node() {
            match parse_node_result {
                Ok(node) => nodes.push(node),
                Err(error) => return Err(error)
            }
        }
   
        Ok(nodes)
    }
    
    fn parse_node(&mut self) -> Option<ParseNodeResult> {
        let token_result = self.tokens.advance()?;
        match token_result {
            Ok(token) => {
                self.span = token.span;
                match token.kind {
                    TokenKind::Number => self.number(),
                    TokenKind::Character => self.character(),
                    TokenKind::String => self.string(),
                    TokenKind::Plus => self.operator(Op::Plus),
                    TokenKind::Minus => self.operator(Op::Minus),
                    TokenKind::Star => self.operator(Op::Multiply),
                    TokenKind::Slash => self.operator(Op::Divide),
                    TokenKind::OpenBracket => self.array(),
                    TokenKind::CloseBracket => {
                        Some(Err(ParseError::new(ParseErrorKind::Unexpected(token.kind), self.span.clone())))
                    }
                }
            },
            Err(lex_error) => {
                self.span = lex_error.span;
                Some(Err(ParseError::new(Lex(lex_error.kind), self.span.clone())))
            }
        }
    }
    
    fn array(&mut self) -> Option<ParseNodeResult> {
        let mut elements: Vec<Node> = Vec::new();
        let mut last_span = self.span.clone();
    
        loop {
            match self.tokens.peek() {
                None => return Some(Err(ParseError::new(ParseErrorKind::UnterminatedArray, last_span))),
                Some(Err(error)) => return Some(Err(ParseError::new(Lex(error.kind.clone()), last_span))),
                Some(Ok(token)) => {
                    last_span = token.span.clone();

                    match token.kind {
                        TokenKind::CloseBracket => break,
                        _ => match self.parse_node().unwrap() {
                            Ok(element) => elements.push(element),
                            Err(error) => return Some(Err(error))
                        }
                    }
                }
            }
        }
    
        self.tokens.advance();

        self.span = self.span.clone() + last_span;
        Some(Ok(self.node(NodeValue::Array(elements))))
    }
    
    fn string(&mut self) -> Option<ParseNodeResult> {
        let str: EcoString = self.lexeme(&self.span).into();
        Some(Ok(self.node(NodeValue::String(str))))
    }
    
    fn character(&mut self) -> Option<ParseNodeResult> {
        let c = self.lexeme(&self.span).chars().next().unwrap();
        Some(Ok(self.node(NodeValue::Character(c))))
    }
    
    fn number(&mut self) -> Option<ParseNodeResult> {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        Some(Ok(self.node(Integer(number))))
    }

    fn operator(&self, operator: Op) -> Option<ParseNodeResult> {
        Some(Ok(self.node(Operator(operator))))
    }

    fn node(&self, value: NodeValue) -> Node {
        Node::new(value, self.span.clone())
    }

    fn error(&self, error: ParseErrorKind) -> ParseError {
        ParseError::new(error, self.span.clone())
    }
    
    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }
}

pub fn parse(source: &str) -> ParseResult {
    let tokens = tokenize(source);
    let mut parser = Parser::new(source, tokens);
    parser.parse()
}

