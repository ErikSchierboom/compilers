use crate::lex::{tokenize, LexErrorKind, Span, TokenKind, TokenResult};
use crate::parse::NodeValue::{Integer, Operator};
use crate::parse::ParseErrorKind::Lex;
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexErrorKind),
    Unexpected(TokenKind),
    UnterminatedArray,
}

#[derive(Debug)]
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
        match self.tokens.advance()? {
            Ok(token) => {
                self.span = token.span;
                match token.kind {
                    TokenKind::Number => self.integer(),
                    TokenKind::Plus => self.operator(Op::Plus),
                    TokenKind::Minus => self.operator(Op::Minus),
                    TokenKind::Star => self.operator(Op::Multiply),
                    TokenKind::Slash => self.operator(Op::Divide),
                    TokenKind::OpenBracket => self.array(),
                    TokenKind::CloseBracket => self.error(ParseErrorKind::Unexpected(token.kind))
                }
            },
            Err(lex_error) => {
                self.span = lex_error.span;
                self.error(Lex(lex_error.kind))
            }
        }
    }

    fn integer(&mut self) -> Option<ParseNodeResult> {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.node(Integer(number))
    }

    fn operator(&self, operator: Op) -> Option<ParseNodeResult> {
        self.node(Operator(operator))
    }

    fn array(&mut self) -> Option<ParseNodeResult> {
        let mut elements: Vec<Node> = Vec::new();

        loop {
            match self.tokens.advance() {
                None => return self.error(ParseErrorKind::UnterminatedArray),
                Some(Err(lex_error)) => {
                    self.span = lex_error.span;
                    return self.error(Lex(lex_error.kind))
                }
                Some(Ok(token)) => {
                    match token.kind {
                        TokenKind::CloseBracket => {
                            self.tokens.advance();
                            break
                        },
                        _ => match self.parse_node().unwrap() {
                            Ok(element) => elements.push(element),
                            Err(error) => return Some(Err(error))
                        }
                    }
                }
            }
        }

        self.span = elements.last().map_or(self.span.clone(), |s| s.span.clone());
        self.node(NodeValue::Array(elements))
    }

    fn node(&self, value: NodeValue) -> Option<ParseNodeResult> {
        Some(Ok(Node::new(value, self.span.clone())))
    }

    fn error(&self, error: ParseErrorKind) -> Option<ParseNodeResult> {
        Some(Err(ParseError::new(error, self.span.clone())))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }
}

pub fn parse(source: &str) -> ParseResult {
    let tokens = tokenize(source);
    Parser::new(source, tokens).parse()
}
