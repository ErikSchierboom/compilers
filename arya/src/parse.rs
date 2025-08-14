use std::iter::Peekable;
use ecow::EcoString;
use crate::lex::{tokenize, LexError, LexResult, Span, Token, TokenKind};

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    Unexpected(Token)
}

pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span
}

#[derive(Debug)]
pub enum NodeValue {
    Number(i64),
    Character(char),
    String(EcoString),
    Operator(Operator),
    Array(Vec<Node>)
}

#[derive(Debug)]
pub struct Node {
    pub value: NodeValue,
    pub span: Span
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide
}

struct TokenWindow<T: Iterator<Item = LexResult>> {
    tokens: Peekable<T>
}

impl<T> TokenWindow<T> where T : Iterator<Item = LexResult> {
    pub fn new(tokens: T) -> Self {
        TokenWindow { tokens: tokens.peekable() }
    }

    pub fn peek(&mut self) -> Option<&LexResult> {
        self.tokens.peek()
    }

    pub fn advance(&mut self) -> Option<LexResult> {
        self.advance_if(|_| true)
    }

    pub fn advance_if(&mut self, func: impl Fn(&TokenKind) -> bool) -> Option<LexResult> {
        self.tokens.next_if(|lex_result| {
            match lex_result {
                Ok(token) => func(&token.kind),
                _ => false
            }
        })
    }
}

type ParseResult = Result<Vec<Node>, ParseError>;

pub struct Parser<'a, T> where T : Iterator<Item = LexResult> {
    tokens: TokenWindow<T>,
    source_code: &'a str
}

impl<'a, T> Parser<'a, T> where T : Iterator<Item = LexResult> {
    pub fn new(source: &'a str, tokens: T) -> Self {
        Parser { tokens: TokenWindow::new(tokens), source_code: source }
    }

    pub fn parse(&mut self) -> ParseResult {
        todo!("")
    //
    //
    //     let mut nodes: Vec<Spanned<Node>> = Vec::new();
    //     let mut errors: Vec<Spanned<ParseError>> = Vec::new();
    //
    //     while let Some(parse_result) = self.parse_node() {
    //         match parse_result {
    //             Ok(node) => nodes.push(node),
    //             Err(error) => errors.push(error)
    //         }
    //     }
    //
    //     if errors.len() > 0 {
    //         Err(errors)
    //     } else {
    //         Ok(nodes)
    //     }
    // }
    //
    // fn parse_node(&mut self) -> Option<ParseNodeResult> {
    //     let spanned_token = self.tokens.advance()?;
    //     match spanned_token.value {
    //         Token::Number => self.number(spanned_token),
    //         Token::Character => self.character(spanned_token),
    //         Token::String => self.string(spanned_token),
    //         Token::Identifier => self.identifier(spanned_token),
    //         Token::OpenBracket => self.array(spanned_token),
    //         Token::CloseBracket => Some(Err(spanned_token.with_value(ParseError::Unexpected(Token::CloseBracket))))
    //     }
    // }
    //
    // fn array(&mut self, open_bracket_token: Spanned<Token>) -> Option<ParseNodeResult> {
    //     let mut elements: Vec<Node> = Vec::new();
    //     let mut last_span = open_bracket_token.span.clone();
    //
    //     loop {
    //         match self.tokens.peek() {
    //             None => return Some(Err(Spanned::new(ParseError::Expected(Token::CloseBracket), last_span))),
    //             Some(Spanned { value: Token::CloseBracket, span}) => {
    //                 last_span = span.clone();
    //                 break
    //             }
    //             Some(spanned_token) => {
    //                 last_span = spanned_token.span.clone();
    //
    //                 match self.parse_node().unwrap() {
    //                     Ok(element) => elements.push(element.value),
    //                     Err(error) => return Some(Err(error))
    //                 }
    //             }
    //         }
    //     }
    //
    //     self.tokens.advance();
    //
    //     Some(Ok(Spanned::new(Node::Array(elements), open_bracket_token.span + last_span)))
    // }
    //
    // fn string(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
    //     let str = self.lexeme(&spanned_token.span).to_string();
    //     Some(Ok(spanned_token.with_value(Node::String(str))))
    // }
    //
    // fn character(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
    //     // TODO: check if we need error handling
    //     let c = self.lexeme(&spanned_token.span).chars().next().unwrap();
    //     Some(Ok(spanned_token.with_value(Node::Character(c))))
    // }
    //
    // fn number(&mut self, spanned_token: Spanned<Token>) -> Option<ParseNodeResult> {
    //     // TODO: check if we need error handling
    //     let number = i64::from_str(self.lexeme(&spanned_token.span)).unwrap();
    //     Some(Ok(spanned_token.with_value(Node::Number(number))))
    // }
    //
    // fn lexeme(&self, span: &Span) -> &'a str {
    //     let begin = span.begin.position as usize;
    //     let end = span.end.position as usize;
    //     &self.source_code[begin..end]
    }
}

pub fn parse(source: &str) -> ParseResult {
    let tokens = tokenize(source);
    let mut parser = Parser::new(source, tokens);
    parser.parse()
}

