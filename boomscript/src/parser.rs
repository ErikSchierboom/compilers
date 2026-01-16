use crate::lexer::{tokenize, LexicalError, Token, TokenKind};
use crate::location::Span;
use std::iter::Peekable;

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
    span: Span,
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
        span: Span,
    },
    Expr {
        value: UntypedExpr,
        span: Span,
    },
    Fn {
        name: String,
        parameters: Vec<String>,
        body: Vec<UntypedStmt>,
    },
}

#[derive(Debug)]
pub enum UntypedExpr {
    Int {
        value: i64,
        span: Span,
    },
    Float {
        value: f64,
        span: Span,
    },
    Char {
        value: char,
        span: Span,
    },
    String {
        value: String,
        span: Span,
    },
    Var {
        name: String,
        span: Span,
    },
    Call {
        func: Box<Self>,
        arguments: Vec<Box<Self>>,
        span: Span,
    },
    Binary {
        left: Box<Self>,
        op: BinaryOperator,
        right: Box<Self>,
        span: Span,
    },
    Unary {
        value: Box<Self>,
        op: UnaryOperator,
        span: Span,
    },
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
            UntypedExpr::Binary { span, .. } |
            UntypedExpr::Unary { span, .. } => span
        }
    }
}

#[derive(Debug)]
pub enum BinaryOperator {
    // Numeric
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum UnaryOperator {
    // Logical
    Not,

    // Numeric
    Neg,
}

impl UnaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::Not => 30,
            UnaryOperator::Neg => 30,
        }
    }
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add => 10,
            BinaryOperator::Sub => 10,
            BinaryOperator::Mul => 20,
            BinaryOperator::Div => 20
        }
    }
}

struct Parser<'a, T: Iterator<Item=Token>> {
    code: &'a str,
    tokens: Peekable<T>,
}

type PrefixParser<'a, T> = fn(&mut Parser<'a, T>, Token) -> Result<UntypedExpr, ParseError>;
type InfixParser<'a, T> = fn(&mut Parser<'a, T>, UntypedExpr, Token) -> Result<UntypedExpr, ParseError>;

impl<'a, T: Iterator<Item=Token>> Parser<'a, T> {
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
        match self.parse_expression(0)? {
            Ok(expr) => {
                let span = expr.span().clone();
                Some(Ok(UntypedStmt::Expr { value: expr, span }))
            }
            Err(error) => Some(Err(error))
        }
    }

    fn parse_expression(&mut self, precedence: u8) -> Option<Result<UntypedExpr, ParseError>> {
        let token = self.tokens.next()?;
        let prefix_parser = self.prefix_parser(&token.kind).unwrap(); // TODO: error handling

        match prefix_parser(self, token) {
            Ok(left) => {
                let mut left = left;

                self.tokens.peek().map(|token| token.)

                while (precedence < getPrecedence()) {
                    token = consume();

                    InfixParselet infix = mInfixParselets.get(token.getType());
                    left = infix.parse(this, left, token);
                }

                return left;
            }
            Err(err) => Some(Err(err))
        }


    }

    // TODO: maybe create a type for the prefix parser
    fn prefix_parser(&self, token_kind: &TokenKind) -> Option<PrefixParser<'a, T>> {
        match &token_kind {
            TokenKind::Int => Some(Self::parse_int),
            TokenKind::Float => Some(Self::parse_float),
            TokenKind::Char => Some(Self::parse_char),
            TokenKind::String => Some(Self::parse_string),
            TokenKind::Minus => Some(Self::parse_unary_minus),
            _ => None
        }
    }

    fn infix_parser(&self, token_kind: &TokenKind) -> Option<InfixParser<'a, T>> {
        match &token_kind {
            TokenKind::Plus => Some(Self::parse_add),
            TokenKind::Minus => Some(Self::parse_sub),
            TokenKind::Star => Some(Self::parse_mul),
            TokenKind::Slash => Some(Self::parse_div),
            _ => None
        }
    }

    fn parse_int(&mut self, token: Token) -> Result<UntypedExpr, ParseError> {
        let value = self.lexeme(&token.span).parse().unwrap();
        Ok(UntypedExpr::Int { value, span: token.span })
    }

    fn parse_float(&mut self, token: Token) -> Result<UntypedExpr, ParseError> {
        let value = self.lexeme(&token.span).parse().unwrap();
        Ok(UntypedExpr::Float { value, span: token.span })
    }

    fn parse_char(&mut self, token: Token) -> Result<UntypedExpr, ParseError> {
        let value = match self.lexeme(&token.span) {
            "'\\n'" => '\n',
            "'\\r'" => '\r',
            "'\\t'" => '\t',
            "'\\''" => '\'',
            lexeme => lexeme.chars().nth(1).unwrap()
        };
        Ok(UntypedExpr::Char { value, span: token.span })
    }

    fn parse_string(&mut self, token: Token) -> Result<UntypedExpr, ParseError> {
        let value = String::from(&self.lexeme(&token.span)[1..token.span.end as usize - token.span.start as usize - 1])
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\r", "\r")
            .replace("\\\"", "\"");
        Ok(UntypedExpr::String { value, span: token.span })
    }

    fn parse_unary_minus(&mut self, token: Token) -> Result<UntypedExpr, ParseError> {
        self.parse_unary_op(token, UnaryOperator::Neg)
    }

    fn parse_unary_op(&mut self, token: Token, op: UnaryOperator) -> Result<UntypedExpr, ParseError> {
        match self.parse_expression(op.precedence()) {
            None => {
                Err(ParseError { kind: ParseErrorKind::UnexpectedEndOfFile, span: token.span })
            },
            Some(result) => {
                let value = result?;
                Ok(UntypedExpr::Unary { value: Box::new(value), op, span: token.span })
            }
        }
    }

    fn parse_add(&mut self, left: UntypedExpr, token: Token) -> Result<UntypedExpr, ParseError> {
        self.parse_binary_op(left, token, BinaryOperator::Add)
    }

    fn parse_sub(&mut self, left: UntypedExpr, token: Token) -> Result<UntypedExpr, ParseError> {
        self.parse_binary_op(left, token, BinaryOperator::Sub)
    }

    fn parse_mul(&mut self, left: UntypedExpr, token: Token) -> Result<UntypedExpr, ParseError> {
        self.parse_binary_op(left, token, BinaryOperator::Mul)
    }

    fn parse_div(&mut self, left: UntypedExpr, token: Token) -> Result<UntypedExpr, ParseError> {
        self.parse_binary_op(left, token, BinaryOperator::Div)
    }

    fn parse_binary_op(&mut self, left: UntypedExpr, token: Token, op: BinaryOperator) -> Result<UntypedExpr, ParseError> {
        match self.parse_expression(op.precedence()) {
            None => {
                Err(ParseError { kind: ParseErrorKind::UnexpectedEndOfFile, span: token.span })
            },
            Some(result) => {
                let right = result?;
                Ok(UntypedExpr::Binary { left: Box::new(left), right: Box::new(right), op, span: token.span })
            }
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
