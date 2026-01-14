use std::iter::Peekable;
use crate::lexer::{tokenize, LexicalError, Token, TokenKind};
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
    Call {
        func: Box<Self>,
        arguments: Vec<Box<Self>>,
        span: Span
    },
    Binary {
        left: Box<Self>,
        right: Box<Self>,
        span: Span
    }
}

impl UntypedExpr {
    fn span(&self) -> &Span {
        match self {
            UntypedExpr::Int { span, .. } |
            UntypedExpr::Float { span, .. } |
            UntypedExpr::Char { span, .. } |
            UntypedExpr::String { span, .. } |
            UntypedExpr::Var { span, .. } |
            UntypedExpr::Call { span, .. } |
            UntypedExpr::Binary { span, .. } => span
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    // Math operators
    Add,
    Sub,
    Mul,
    Div
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

    fn parse(&mut self) -> Result<Vec<UntypedStmt>, Vec<ParseError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();

        while let Some(result) = self.parse_statement() {
            match result {
                Ok(word) => statements.push(word),
                Err(parse_error) => errors.push(parse_error)
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn parse_statement(&mut self) -> Option<Result<UntypedStmt, ParseError>> {
        let Token { kind, .. } = self.tokens.peek()?;

        match kind {
            TokenKind::Fn => self.parse_function_statement(),
            _ => self.parse_expression_statement()
        }
    }

    fn parse_function_statement(&mut self) -> Option<Result<UntypedStmt, ParseError>> {
        todo!("parse fn")
    }

    fn parse_expression_statement(&mut self) -> Option<Result<UntypedStmt, ParseError>> {
        match self.parse_expression()? {
            Ok(expr) => {
                let span = expr.span().clone();
                Some(Ok(UntypedStmt::Expr { value: expr, span }))
            }
            Err(error) => Some(Err(error))
        }
    }

    fn parse_expression(&mut self) -> Option<Result<UntypedExpr, ParseError>> {
        let Token { kind, span } = self.tokens.next()?;

        match &kind {
            TokenKind::Int => {
                let value = self.lexeme(&span).parse().unwrap();
                Some(Ok(UntypedExpr::Int { value, span }))
            }
            TokenKind::Float => {
                let value = self.lexeme(&span).parse().unwrap();
                Some(Ok(UntypedExpr::Float { value, span }))
            }
            TokenKind::Char => {
                let value = match self.lexeme(&span) {
                    "'\\n'" => '\n',
                    "'\\r'" => '\r',
                    "'\\t'" => '\t',
                    "'\\''" => '\'',
                    lexeme => lexeme.chars().nth(1).unwrap()
                };
                Some(Ok(UntypedExpr::Char { value, span }))
            }
            TokenKind::String => {
                let value = String::from(&self.lexeme(&span)[1..span.end as usize - span.start as usize - 1])
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\"", "\"");
                Some(Ok(UntypedExpr::String { value, span }))
            }

            _ => todo!("parse other expressions")
            // TODO: implement Pratt parser
         
            // TokenKind::Word => {
            //     let name = self.lexeme(&location).into();
            //     Some(Ok(Spanned::new(UntypedExpr::Word(name), location)))
            // }
        }
    }

    fn lexeme(&self, location: &Span) -> &'a str {
        &self.code[location.start as usize..location.end as usize]
    }
}

pub fn parse(code: &str) -> Result<Vec<UntypedStmt>, Vec<ParseError>> {
    match tokenize(code) {
        Ok(tokens) => Parser::new(code, tokens.into_iter()).parse(),
        Err(errors) => Err(errors.into_iter().map(ParseError::from).collect()),
    }
}
