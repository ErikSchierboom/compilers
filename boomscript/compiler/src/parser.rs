use crate::lexer::{tokenize, LexError, Token};
use std::iter::Peekable;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
    ExpectedIdentifier,
    UnexpectedEndOfFile,
}

#[derive(Debug)]
pub enum Word {
    Number(i64),
    Char(char),
    String(String),
    Identifier(String),
    Quote(String),
    Block(Vec<Word>),
}

struct Parser<T>
where
    T: Iterator<Item=Token>,
{
    tokens: Peekable<T>,
}

impl<T> Parser<T>
where
    T: Iterator<Item=Token>,
{
    fn new(tokens: T) -> Self {
        Self { tokens: tokens.peekable() }
    }

    pub fn parse(&mut self) -> Result<Vec<Word>, ParseError> {
        let mut words: Vec<Word> = Vec::new();

        while self.tokens.peek() != Some(&Token::EndOfFile) {
            words.push(self.parse_word()?)
        }

        Ok(words)
    }

    pub fn parse_word(&mut self) -> Result<Word, ParseError> {
        let word = match self.advance()? {
            Token::Number(number) => Word::Number(number),
            Token::Char(c) => Word::Char(c),
            Token::String(string) => Word::String(string),
            Token::Identifier(name) => Word::Identifier(name),
            Token::Quote => {
                match self.advance()? {
                    Token::Identifier(name) => Word::Quote(name),
                    _ => return Err(ParseError::ExpectedIdentifier)
                }
            },
            Token::Less => Word::Identifier("lt".to_string()),
            Token::LessEqual => Word::Identifier("le".to_string()),
            Token::Greater => Word::Identifier("gt".to_string()),
            Token::GreaterEqual => Word::Identifier("ge".to_string()),
            Token::OpenBracket => {
                let mut words: Vec<Word> = Vec::new();
                
                loop {
                    if self.matches(&Token::CloseBracket) {
                        return Ok(Word::Block(words))
                    } else {
                        let block_word = self.parse_word()?;
                        words.push(block_word)
                    }
                }
            }
            Token::CloseBracket => return Err(ParseError::UnexpectedToken(Token::CloseBracket)),
            Token::EndOfFile => panic!("We should never get here")
        };

        Ok(word)
    }

    fn matches(&mut self, expected: &Token) -> bool {
        self.tokens.next_if_eq(&expected).is_some()
    }

    fn matches_any(&mut self, expected: &[Token]) -> Option<Token> {
        expected.into_iter().find_map(|expected_token| self.tokens.next_if_eq(&expected_token))
    }

    fn advance(&mut self) -> Result<Token, ParseError> {
        match self.tokens.next() {
            None => Err(ParseError::UnexpectedEndOfFile),
            Some(token) => Ok(token)
        }
    }

    fn expect(&mut self, token: &Token) -> Result<(), ParseError> {
        match self.tokens.next_if_eq(token) {
            None => Err(ParseError::ExpectedToken(token.clone())),
            Some(_) => Ok(())
        }
    }
}

pub fn parse(source_code: &str) -> Result<Vec<Word>, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
