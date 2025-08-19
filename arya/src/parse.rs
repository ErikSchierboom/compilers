use crate::lex::{tokenize, LexError, Span, Spanned, Token, TokenResult};
use crate::parse::Node::{Integer, Operator};
use crate::parse::ParseError::Lex;
use std::iter::Peekable;
use std::str::FromStr;

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

    pub fn advance_if(&mut self, func: impl Fn(&Token) -> bool) -> Option<TokenResult> {
        self.tokens.next_if(|lex_result| {
            match lex_result {
                Ok(token) => func(&token.value),
                _ => false
            }
        })
    }
    
    pub fn peek(&mut self) -> Option<&TokenResult> {
        self.tokens.peek()
    }
}

type ParseNodeResult = Result<Spanned<Node>, Spanned<ParseError>>;
pub type ParseResult = Result<Vec<Spanned<Node>>, Spanned<ParseError>>;

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
        let mut nodes: Vec<Spanned<Node>> = Vec::new();
        
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
        let mut elements: Vec<Spanned<Node>> = Vec::new();

        loop {
            match self.tokens.peek() {
                None => return self.error(ParseError::UnterminatedArray),
                Some(Err(lex_error)) => {
                    self.span = lex_error.span.clone();
                    let error = Lex(lex_error.value.clone());
                    self.tokens.advance();
                    return self.error(error)
                }
                Some(Ok(token)) => {
                    match token.value {
                        Token::CloseBracket => {
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
        self.node(Node::Array(elements))
    }

    fn node(&self, node: Node) -> Option<ParseNodeResult> {
        Some(Ok(Spanned::new(node, self.span.clone())))
    }

    fn error(&self, error: ParseError) -> Option<ParseNodeResult> {
        Some(Err(Spanned::new(error, self.span.clone())))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }
}

pub fn parse(source: &str) -> ParseResult {
    let tokens = tokenize(source);
    Parser::new(source, tokens).parse()
}
