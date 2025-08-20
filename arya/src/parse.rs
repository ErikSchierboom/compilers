use crate::lex::{tokenize, LexError, Token, ParseTokenResult};
use crate::parse::Node::{Integer, Operator};
use crate::parse::ParseError::Lex;
use std::iter::Peekable;
use std::str::FromStr;
use crate::location::{Span, Spanned};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    Unexpected(Token),
    UnterminatedArray,
}

#[derive(Debug)]
pub enum Node {
    Integer(i64),
    Operator(Op),
    Array(Vec<Spanned<Node>>)
}

#[derive(Debug)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide
}

pub type ParseNodeResult = Result<Spanned<Node>, Spanned<ParseError>>;

pub struct Parser<'a, T> where T : Iterator<Item =ParseTokenResult> {
    tokens: Peekable<T>,
    source_code: &'a str,
    span: Span
}

impl<'a, T> Parser<'a, T> where T : Iterator<Item =ParseTokenResult> {
    fn new(source_code: &'a str, tokens: T) -> Self {
        Parser { tokens: tokens.peekable(), source_code, span: Span::new(0, 0) }
    }

    fn parse_node(&mut self) -> Option<ParseNodeResult> {
        match self.tokens.next()? {
            Ok(token) => {
                self.span = token.span;
                match token.value {
                    Token::Number => self.integer(),
                    Token::Plus => self.operator(Op::Plus),
                    Token::Minus => self.operator(Op::Minus),
                    Token::Star => self.operator(Op::Multiply),
                    Token::Slash => self.operator(Op::Divide),
                    Token::OpenBracket => self.array(),
                    Token::CloseBracket => self.error(ParseError::Unexpected(token.value))
                }
            },
            Err(lex_error) => {
                self.span = lex_error.span;
                self.error(Lex(lex_error.value))
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
        let start_span = self.span.clone();
        let mut elements: Vec<Spanned<Node>> = Vec::new();
        
        loop {
            match self.tokens.peek() {
                None => return self.error(ParseError::UnterminatedArray),
                Some(Err(lex_error)) => {
                    self.span = lex_error.span.clone();
                    let error = Lex(lex_error.value.clone());
                    self.tokens.next();
                    return self.error(error)
                }
                Some(Ok(token)) => {
                    match token.value {
                        Token::CloseBracket => {
                            self.span = start_span.merge(&token.span);
                            self.tokens.next(); // consume the closing bracket         
                            return self.node(Node::Array(elements))
                        },
                        _ => match self.parse_node().unwrap() {
                            Ok(element) => elements.push(element),
                            Err(error) => return Some(Err(error))
                        }
                    }
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

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }
}

impl<'a, T> Iterator for Parser<'a, T> where T : Iterator<Item =ParseTokenResult> {
    type Item = ParseNodeResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_node()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item=ParseNodeResult> + '_ {
    let tokens = tokenize(source);
    Parser::new(source, tokens)
}
