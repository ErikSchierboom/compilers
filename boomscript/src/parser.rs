use crate::lexer::{tokenize, LexError, Token, TokenKind};
use crate::location::Span;
use std::iter::Peekable;
use crate::parser::ParseErrorKind::Lex;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedEndOfFile,
    UnexpectedIdentifier(String),
    ExpectedIdentifier,
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    location: Span,
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        let location = value.location.clone();
        Self { kind: Lex(value), location }
    }
}

#[derive(Clone, Debug)]
pub enum BuiltinKind {
    Dup,
    Drop,
    Swap,
    Over,
}

impl TryFrom<&str> for BuiltinKind {
    type Error = ParseErrorKind;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "dup" => Ok(BuiltinKind::Dup),
            "drop" => Ok(BuiltinKind::Drop),
            "swap" => Ok(BuiltinKind::Swap),
            "over" => Ok(BuiltinKind::Over),
            _ => Err(ParseErrorKind::UnexpectedIdentifier(value.to_string()))
        }
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, location: Span },
    Quote { name: String, location: Span },
    Builtin { kind: BuiltinKind, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { elements: Vec<Word>, location: Span },

    // Binary operators
    Add { location: Span },
    Mul { location: Span },

    // Memory operators
    Read { variable: Option<String>, location: Span },
    Write { variable: Option<String>, location: Span },
    Execute { variable: Option<String>, location: Span },
}

struct Parser<'a, T: Iterator<Item=Token>> {
    code: &'a str,
    tokens: Peekable<T>,
    token: Option<Token>,
    words: Vec<Word>,
}

impl<'a, T: Iterator<Item=Token>> Parser<'a, T> {
    fn new(code: &'a str, tokens: T) -> Self {
        Self { code, tokens: tokens.peekable(), token: None, words: Vec::new() }
    }

    fn parse(mut self) -> Result<Vec<Word>, ParseError> {
        while let Some(word) = self.parse_word() {
            self.words.push(word?);
        }

        Ok(self.words)
    }

    fn parse_word(&mut self) -> Option<Result<Word, ParseError>> {
        let token = self.tokens.next()?;
        let location = token.location.clone();

        let result = match &token.kind {
            TokenKind::Int => Ok(Word::Int { value: self.code[location.start..location.end].parse().unwrap(), location }),
            TokenKind::Quote => {
                match self.tokens.next() {
                    Some(Token { kind: TokenKind::Identifier, .. }) => {
                        let name = self.code[token.location.start..token.location.end].to_string();
                        Ok(Word::Quote { name, location })
                    }
                    Some(token) => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location: token.location }),
                    None => Err(ParseError { kind: ParseErrorKind::ExpectedIdentifier, location }),
                }
            }
            TokenKind::Identifier => {
                match BuiltinKind::try_from(&self.code[location.start..location.end]) {
                    Ok(builtin_kind) => Ok(Word::Builtin { kind: builtin_kind, location }),
                    Err(parse_error_kind) => Err(ParseError { kind: parse_error_kind, location })
                }
            }
            TokenKind::Add => Ok(Word::Add { location }),
            TokenKind::Mul => Ok(Word::Mul { location }),

            // TODO: check if followed by identifier
            TokenKind::Read => {
                // TODO: DRY duplicate code
                let variable = if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::Identifier) {
                    Some(self.code[token.location.start..token.location.end].to_string())
                } else {
                    None
                };

                Ok(Word::Read { variable, location })
            }
            TokenKind::Write => {
                let variable = if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::Identifier) {
                    Some(self.code[token.location.start..token.location.end].to_string())
                } else {
                    None
                };

                Ok(Word::Write { variable, location })
            }
            TokenKind::Execute => {
                let variable = if let Some(token) = self.tokens.next_if(|token| token.kind == TokenKind::Identifier) {
                    Some(self.code[token.location.start..token.location.end].to_string())
                } else {
                    None
                };

                Ok(Word::Execute { variable, location })
            }

            TokenKind::OpenBracket => self.parse_array(),
            TokenKind::OpenParen => self.parse_block(),

            _ => Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind.clone()), location })
        };

        Some(result)
    }

    fn parse_block(&mut self) -> Result<Word, ParseError> {
        todo!("parse block")
        // self.next_token();
        //
        // let mut words = Vec::new();
        //
        // loop {
        //     // TODO: use correct location
        //     match &self.token {
        //         Some(Token { kind: TokenKind::CloseParen, .. }) => return Ok(Word::Block { words, location: Span::EMPTY }),
        //         Some(_) => {
        //             let word = self.parse_word()?;
        //             words.push(word);
        //         }
        //         None => return Err(ParseError { kind: ParseErrorKind::ExpectedToken(TokenKind::CloseParen), location: Span::EMPTY })
        //     }
        // }
    }

    fn parse_array(&mut self) -> Result<Word, ParseError> {
        todo!("parse array")
        // self.next_token();
        //
        // let mut elements = Vec::new();
        //
        // loop {
        //     // TODO: use correct location
        //     match &self.token {
        //         Some(Token { kind: TokenKind::CloseBracket, .. }) => return Ok(Word::Array { elements, location: Span::EMPTY }),
        //         Some(_) => {
        //             let word = self.parse_word()?;
        //             elements.push(word);
        //         }
        //         None => return Err(ParseError { kind: ParseErrorKind::ExpectedToken(TokenKind::CloseBracket), location: Span::EMPTY })
        //     }
        // }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    match tokenize(code) {
        Ok(tokens) => {
            let parser = Parser::new(code, tokens.into_iter());
            parser.parse()
        }
        Err(lex_error) => Err(lex_error.into())
    }
}
