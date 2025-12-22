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
        match self.tokens.next() {
            Some(token) => {
                self.token = Some(token);
                self.token.as_ref()
            }
            None => {
                self.token = None;
                None
            }
        }
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
        while self.token.is_some() {
            let word = self.parse_word()?;
            self.words.push(word);
        }

        Ok(self.words)
    }

    fn parse_word(&mut self) -> Result<Word, ParseError> {
        let token = self.token.as_ref().unwrap();
        let location = token.location.clone();

        let result = match &token.kind {
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

            TokenKind::OpenBracket => self.parse_array(),
            TokenKind::OpenParen => self.parse_block(),

            _ => Err(ParseError { kind: ParseErrorKind::UnexpectedToken(token.kind.clone()), location })
        };

        self.next_token();

        result
    }

    fn parse_block(&mut self) -> Result<Word, ParseError> {
        self.next_token();

        let mut words = Vec::new();

        loop {
            // TODO: use correct location
            match &self.token {
                Some(Token { kind: TokenKind::CloseParen, .. }) => return Ok(Word::Block { words, location: Span::EMPTY }),
                Some(_) => {
                    let word = self.parse_word()?;
                    words.push(word);
                }
                None => return Err(ParseError { kind: ParseErrorKind::ExpectedToken(TokenKind::CloseParen), location: Span::EMPTY })
            }
        }
    }

    fn parse_array(&mut self) -> Result<Word, ParseError> {
        self.next_token();

        let mut elements = Vec::new();

        loop {
            // TODO: use correct location
            match &self.token {
                Some(Token { kind: TokenKind::CloseBracket, .. }) => return Ok(Word::Array { elements, location: Span::EMPTY }),
                Some(_) => {
                    let word = self.parse_word()?;
                    elements.push(word);
                }
                None => return Err(ParseError { kind: ParseErrorKind::ExpectedToken(TokenKind::CloseBracket), location: Span::EMPTY })
            }
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<Word>, ParseError> {
    match tokenize(code) {
        Ok(tokens) => {
            let parser = Parser::new(tokens.into_iter());
            parser.parse()
        }
        Err(lex_error) => {
            let location = lex_error.location.clone();
            let kind = ParseErrorKind::Lex(lex_error);
            Err(ParseError { kind, location })
        }
    }
}
