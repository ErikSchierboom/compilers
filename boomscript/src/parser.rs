use std::iter::Peekable;
use crate::lexer::{tokenize, LexicalError, Token};
use crate::location::Span;

// use crate::lexer::{tokenize, LexError, Token};
// use crate::location::{Span, Spanned};
// use crate::parser::ParseError::Lex;
// use std::fmt;
// use std::fmt::{Display, Formatter};
// use std::iter::Peekable;
//
#[derive(Debug)]
pub enum ParseErrorKind {
    Lexical(LexicalError),
    UnexpectedToken(String),
    UnexpectedEndOfFile,
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span
}

impl ParseError {
    fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl From<LexicalError> for ParseError {
    fn from(value: LexicalError) -> Self {
        let span = value.span.clone();
        Self::new(ParseErrorKind::Lexical(value), span)
    }
}

#[derive(Debug)]
pub enum UntypedStmt {
    Assignment {
        name: String,
        value: UntypedExpr,
        span: Span
    },
    Expr {
        value: UntypedExpr,
        span: Span
    },
    Fn {
        name: String,
        parameters: Vec<String>,
        body: Vec<UntypedStmt>
    }
}

#[derive(Debug)]
pub enum UntypedExpr {
    // Literals
    Int {
        value: i64,
        span: Span
    },
    Float {
        value: f64,
        span: Span
    },
    Char {
        value: char,
        span: Span
    },
    String {
        value: String,
        span: Span
    },
    Var {
        name: String,
        span: Span
    },

    // // Composite
    // Block(Vec<UntypedExpr>),
    // Array(Vec<UntypedExpr>),
}

struct Parser<'a, T: Iterator<Item = Token>> {
    code: &'a str,
    tokens: Peekable<T>,
}

impl<'a, T: Iterator<Item = Token>> Parser<'a, T> {
    fn new(code: &'a str, tokens: T) -> Self {
        Self {
            code,
            tokens: tokens.peekable(),
        }
    }

    fn parse(self) -> Result<Vec<UntypedExpr>, Vec<ParseError>> {
        todo!()
        // let mut tokens = Vec::new();
        // let mut errors = Vec::new();
        //
        // while let Some(result) = self.parse_word() {
        //     match result {
        //         Ok(word) => tokens.push(word),
        //         Err(parse_error) => errors.extend(parse_error)
        //     }
        // }
        //
        // if errors.is_empty() {
        //     Ok(tokens)
        // } else {
        //     Err(errors)
        // }
    }

    // fn parse_word(&mut self) -> Option<Result<Spanned<UntypedExpr>, Vec<Spanned<ParseErrorKind>>>> {
    //     let spanned = self.tokens.next()?;
    //     let Spanned { value: token, span: location } = spanned;
    //
    //     match token {
    //         Token::Int => {
    //             let value = self.lexeme(&location).parse().unwrap();
    //             Some(Ok(Spanned::new(UntypedExpr::Int(value), location)))
    //         }
    //         Token::Float => {
    //             let value = self.lexeme(&location).parse().unwrap();
    //             Some(Ok(Spanned::new(UntypedExpr::Float(value), location)))
    //         }
    //         Token::Char => {
    //             let value = match &self.lexeme(&location)[1..] {
    //                 "\\n" => '\n',
    //                 "\\r" => '\r',
    //                 "\\t" => '\t',
    //                 "\\'" => '\'',
    //                 lexeme => lexeme.chars().next().unwrap()
    //             };
    //             Some(Ok(Spanned::new(UntypedExpr::Char(value), location)))
    //         }
    //         Token::String => {
    //             let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
    //                 .replace("\\n", "\n")
    //                 .replace("\\t", "\t")
    //                 .replace("\\r", "\r")
    //                 .replace("\\\"", "\"");
    //             Some(Ok(Spanned::new(UntypedExpr::String(value), location)))
    //         }
    //         Token::Quote => {
    //             let name = self.lexeme(&location)[1..].into();
    //             Some(Ok(Spanned::new(UntypedExpr::QuotedWord(name), location)))
    //         }
    //         Token::Word => {
    //             let name = self.lexeme(&location).into();
    //             Some(Ok(Spanned::new(UntypedExpr::Word(name), location)))
    //         }
    //         Token::OpenBracket => {
    //             match self.parse_delimited(Token::CloseBracket, location) {
    //                 Ok((words, location)) => Some(Ok(Spanned::new(UntypedExpr::Array(words), location))),
    //                 Err(err) => Some(Err(err))
    //             }
    //         }
    //         Token::OpenParen => {
    //             match self.parse_delimited(Token::CloseParen, location) {
    //                 Ok((words, location)) => Some(Ok(Spanned::new(UntypedExpr::Block(words), location))),
    //                 Err(err) => Some(Err(err))
    //             }
    //         }
    //         Token::CloseBracket |
    //         Token::CloseParen => Some(Err(vec![Spanned::new(ParseErrorKind::UnexpectedToken(self.lexeme(&location).into()), location)])),
    //     }
    // }
    //
    // fn parse_delimited(&mut self, close_delimiter: Token, start: Span) -> Result<(Vec<Spanned<UntypedExpr>>, Span), Vec<Spanned<ParseErrorKind>>> {
    //     let mut words = Vec::new();
    //     let mut errors = Vec::new();
    //
    //     loop {
    //         if let Some(token) = self.tokens.next_if(|token| token.value == close_delimiter) {
    //             let location = start.merge(&token.span);
    //             if errors.is_empty() {
    //                 return Ok((words, location));
    //             } else {
    //                 return Err(errors);
    //             }
    //         }
    //
    //         match self.parse_word() {
    //             None => {
    //                 errors.push(Spanned::new(ParseErrorKind::UnexpectedEndOfFile, Span { start: self.code.len(), end: self.code.len() + 1 }));
    //                 return Err(errors);
    //             }
    //             Some(Ok(word)) => words.push(word),
    //             Some(Err(err)) => errors.extend(err)
    //         }
    //     }
    // }
    //
    // fn lexeme(&self, location: &Span) -> &'a str {
    //     &self.code[location.start as usize..location.end as usize]
    // }
}

pub fn parse(code: &str) -> Result<Vec<UntypedExpr>, Vec<ParseError>> {
    match tokenize(code) {
        Ok(tokens) => Parser::new(code, tokens.into_iter()).parse(),
        Err(errors) => Err(errors.into_iter().map(ParseError::from).collect()),
    }
}
