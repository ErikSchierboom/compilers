use crate::lexer::{tokenize, LexError, Span, Token, TokenKind};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseErrorKind {
    Lex(LexError),
    ExpectedToken(TokenKind),
    UnexpectedToken(TokenKind),
    UnexpectedEndOfFile,
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    location: Span,
}

#[derive(Clone, Debug)]
pub enum Word {
    // Literals
    Int { value: i64, location: Span },
    Quote { name: String, location: Span },
    Identifier { name: String, location: Span },

    // Composite
    Block { words: Vec<Word>, location: Span },
    Array { elements: Vec<Word>, location: Span },

    // Binary operators
    Add { location: Span },
    Mul { location: Span },

    // Stack operators
    Dup { location: Span },
    Drop { location: Span },
    Swap { location: Span },
    Over { location: Span },

    // Memory operators
    Read { variable: Option<String>, location: Span },
    Write { variable: Option<String>, location: Span },
    Execute { variable: Option<String>, location: Span },
}

struct Parser<T: Iterator<Item=Token>> {
    tokens: Peekable<T>,
    token: Option<Token>,
    words: Vec<Word>,
}

impl<T: Iterator<Item=Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        let mut parser = Self { tokens: tokens.peekable(), token: None, words: Vec::new() };
        parser.next_token();
        parser
    }

    fn next_token(&mut self) -> Option<&Token> {
        self.next_token_if(|_| true)
    }

    fn next_token_if_kind(&mut self, kind: TokenKind) -> Option<&Token> {
        self.next_token_if(|token| token.kind == kind)
    }

    fn next_token_if(&mut self, f: impl FnOnce(&Token) -> bool) -> Option<&Token> {
        match self.tokens.next_if(f) {
            Some(token) => {
                self.token = Some(token);
                self.token.as_ref()
            }
            None => None
        }
    }

    fn parse(mut self) -> Result<Vec<Word>, ParseError> {
        while let Some(token) = &self.token {
            let word = match &token.kind {
                TokenKind::OpenParen => self.parse_block()?,
                TokenKind::OpenBracket => self.parse_array()?,
                _ => self.parse_expression()?
            };

            self.words.push(word);
        }

        Ok(self.words)
    }

    fn parse_block(&self) -> Result<Word, ParseError> {
        todo!()
    }

    fn parse_array(&self) -> Result<Word, ParseError> {
        let start_pos = self.token.unwrap()
        let mut words = Vec::new();

        loop {
            match self.token {
                None => {
                    let location = words.last().map(|word| word.l)
                    return Err(ParseError { kind: ParseErrorKind::ExpectedToken(TokenKind::CloseBracket), location: location.unwrap_or_default() })
                },
                Some(token) => {
                    match token.kind {
                        TokenKind::CloseBracket => break,
                        _ => {
                            todo!()
                        }
                    }
                }
            }
        }

        // Ok(Word::Array { elements: words))
    }

    fn parse_expression(&self) -> Result<Word, ParseError> {
        match &self.token {
            Some(token) => {
                let location = token.location.clone();

                match &token.kind {
                    TokenKind::Int(value) => Ok(Word::Int { value: value.clone(), location }),
                    TokenKind::Quote(name) => Ok(Word::Quote { name: name.clone(), location }),
                    TokenKind::Identifier(name) => Ok(Word::Identifier { name: name.clone(), location }),
                    TokenKind::Add => Ok(Word::Add { location }),
                    TokenKind::Mul => Ok(Word::Mul { location }),
                    TokenKind::Dup => Ok(Word::Dup { location }),
                    TokenKind::Drop => Ok(Word::Drop { location }),
                    TokenKind::Swap => Ok(Word::Swap { location }),
                    TokenKind::Over => Ok(Word::Over { location }),

                    // TODO: check if followed by identifier
                    TokenKind::Read => Ok(Word::Read { variable: None, location }),
                    TokenKind::Write => Ok(Word::Write { variable: None, location }),
                    TokenKind::Execute => Ok(Word::Execute { variable: None, location }),
                    _ => Err(ParseError { kind: ParseErrorKind::UnexpectedEndOfFile, location })
                }
            },
            None => Err(ParseError { kind: ParseErrorKind::UnexpectedEndOfFile, location: Span { start: 0, end: 0 }})
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    match tokenize(code) {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens.into_iter());
            parser.parse()
        }
        Err(lex_error) => {
            let location = lex_error.location.clone();
            let kind = ParseErrorKind::Lex(lex_error);
            Err(ParseError { kind, location })
        }
    }
}
