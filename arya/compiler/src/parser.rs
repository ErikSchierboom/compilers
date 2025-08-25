use crate::lexer::{tokenize, LexError, ParseTokenResult, Token};
use crate::location::{Span, Spanned};
use crate::parser::Node::{Identifier, Integer, Operation};
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
pub enum Node {
    Integer(i64),
    Identifier(String),
    Operation(Op),
    Array(Vec<Spanned<Node>>),
    Binding(String, Vec<Spanned<Node>>)
}

#[derive(Clone, Debug)]
pub enum Op {
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
    Over
}

pub type ParseNodeResult = Result<Spanned<Node>, Spanned<ParseError>>;

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
            Token::Identifier => match self.next_if_token(&Token::Colon) {
                Some(_) => self.binding(spanned_token),
                None => self.identifier()
            },
            Token::Plus => self.operation(Op::Add),
            Token::Minus => self.operation(Op::Subtract),
            Token::Star => self.operation(Op::Multiply),
            Token::Slash => self.operation(Op::Divide),
            Token::Ampersand => self.operation(Op::And),
            Token::Pipe => self.operation(Op::Or),
            Token::Caret => self.operation(Op::Xor),
            Token::Bang => self.operation(Op::Not),
            Token::Underscore => self.operation(Op::Negate),
            Token::Equal => self.operation(Op::Equal),
            Token::NotEqual => self.operation(Op::NotEqual),
            Token::Greater => self.operation(Op::Greater),
            Token::GreaterEqual => self.operation(Op::GreaterEqual),
            Token::Less => self.operation(Op::Less),
            Token::LessEqual => self.operation(Op::LessEqual),
            Token::Dup => self.operation(Op::Dup),
            Token::Drop => self.operation(Op::Drop),
            Token::Swap => self.operation(Op::Swap),
            Token::Over => self.operation(Op::Over),
            Token::CloseBracket => self.error(ParseError::Unexpected(spanned_token.value)),
            Token::Colon => self.error(ParseError::Unexpected(spanned_token.value)),
            Token::Newline => self.parse_node(),
            Token::Whitespace => self.parse_node(),
            Token::Comment => self.parse_node(),
        }
    }

    fn integer(&mut self) -> Option<ParseNodeResult> {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.node(Integer(number))
    }

    fn identifier(&mut self) -> Option<ParseNodeResult> {
        let name = self.lexeme(&self.span);
        self.node(Identifier(name.to_string()))
    }

    fn operation(&self, operator: Op) -> Option<ParseNodeResult> {
        self.node(Operation(operator))
    }

    fn array(&mut self) -> Option<ParseNodeResult> {
        let start_span = self.span.clone();
        let mut elements: Vec<Spanned<Node>> = Vec::new();

        loop {
            match self.next() {
                None => return self.error(ParseError::UnterminatedArray),
                Some(Err(lex_error)) => return self.error(Lex(lex_error.value)),
                Some(Ok(token)) if token.value == Token::CloseBracket => {
                    self.span = start_span.merge(&self.span);
                    return self.node(Node::Array(elements));
                }
                Some(Ok(token)) => match self.parse_node_from_token(token)? {
                    Err(error) => return Some(Err(error)),
                    Ok(element) => elements.push(element),
                },
            }
        }
    }

    fn binding(&mut self, identifier: Spanned<Token>) -> Option<ParseNodeResult> {
        let mut body: Vec<Spanned<Node>> = Vec::new();

        loop {
            match self.next() {
                None => {
                    self.span = identifier.span.merge(&self.span);
                    return self.node(Node::Binding(self.lexeme(&identifier.span).to_string(), body))
                },
                Some(Ok(token)) if token.value == Token::Newline => {
                    self.span = identifier.span.merge(&self.span);
                    return self.node(Node::Binding(self.lexeme(&identifier.span).to_string(), body))
                }
                Some(Err(lex_error)) => return self.error(Lex(lex_error.value)),
                Some(Ok(token)) => match self.parse_node_from_token(token)? {
                    Err(error) => return Some(Err(error)),
                    Ok(element) => body.push(element),
                }
            }
        }
    }

    fn node(&self, node: Node) -> Option<ParseNodeResult> {
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

    fn next_if_token(&mut self, token: &Token) -> Option<ParseTokenResult> {
        self.tokens
            .next_if(|parse_result| {
                match parse_result {
                    Ok(spanned_token) => &spanned_token.value == token,
                    _ => false
                }
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
