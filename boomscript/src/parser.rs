use crate::lexer::{tokenize, Token};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Word {
    Int(i64),
    Quote(String),
    Block(Vec<Word>),

    Add,
    Mul,
    Read(Option<String>),
    Write(Option<String>),
    Execute(Option<String>),
}

struct Parser<T: Iterator<Item=Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item=Token>> Parser<T> {
    fn new(tokens: T) -> Self {
        Self { tokens: tokens.peekable() }
    }

    fn parse(&mut self) -> Vec<Word> {
        let mut words = Vec::new();

        while let Some(token) = self.tokens.next() {
            match token {
                Token::Int(i) => words.push(Word::Int(i)),
                Token::Quote(word) => words.push(Word::Quote(word)),
                Token::Add => words.push(Word::Add),
                Token::Mul => words.push(Word::Mul),
                Token::Read(identifier) => words.push(Word::Read(identifier)),
                Token::Write(identifier) => words.push(Word::Write(identifier)),
                Token::Execute(identifier) => words.push(Word::Execute(identifier)),
                Token::OpenBracket => words.push(self.parse_block()),
                Token::CloseBracket => panic!("unexpected token")
            }
        }

        words
    }

    fn parse_block(&mut self) -> Word {
        let mut words = Vec::new();

        loop {
            match self.tokens.peek() {
                None => panic!("expected ']'"),
                Some(Token::CloseBracket) => {
                    self.tokens.next();
                    break;
                }
                Some(_) => words.push(self.parse_word().unwrap_or_else(|| panic!("expected word")))
            }
        }

        Word::Block(words)
    }

    fn parse_word(&mut self) -> Option<Word> {
        // TODO: remove duplication
        match self.tokens.next()? {
            Token::Int(i) => Some(Word::Int(i)),
            Token::Quote(word) => Some(Word::Quote(word)),
            Token::Add => Some(Word::Add),
            Token::Mul => Some(Word::Mul),
            Token::Read(identifier) => Some(Word::Read(identifier)),
            Token::Write(identifier) => Some(Word::Write(identifier)),
            Token::Execute(identifier) => Some(Word::Execute(identifier)),
            Token::OpenBracket => todo!("parse block"),
            Token::CloseBracket => panic!("unexpected token")
        }
    }
}

pub fn parse(code: &str) -> Vec<Word> {
    let tokens = tokenize(code);
    let mut parser = Parser::new(tokens.into_iter());
    parser.parse()
}
