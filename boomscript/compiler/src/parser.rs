use crate::lexer::{tokenize, LexError, LexTokenResult, Token};
use crate::location::{Span, Spanned};
use crate::parser::ParseError::Lex;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {token}"),
            ParseError::ExpectedToken(token) => write!(f, "Expected token: {token}"),
        }
    }
}

impl Error for ParseError {}


#[derive(Clone, Debug)]
pub enum Word {
    Char(char),
    Integer(i64),
    String(String),
    Invocation(String),
    Array(Vec<Spanned<Word>>),
    Lambda(Vec<Spanned<Word>>),
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Word::Integer(i) => write!(f, "{i}"),
            Word::Char(c) => write!(f, "'{c}'"),
            Word::String(str) => write!(f, "\"{str}\""),
            Word::Invocation(identifier) => write!(f, "{identifier}()"),
            Word::Array(array) => {
                write!(f, "[")?;
                for (i, element) in array.iter().enumerate() {
                    write!(f, "{}", element)?;
                    if i < array.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            Word::Lambda(_) => write!(f, "lambda")
        }
    }
}

pub type ParseResult<T = Spanned<Word>> = Result<T, Spanned<ParseError>>;

struct Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    source_code: &'a str,
    tokens: Peekable<TTokens>,
    token: Option<LexTokenResult>,
    span: Span,
}

impl<'a, TTokens> Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    fn new(source_code: &'a str, tokens: TTokens) -> Self {
        let mut parser = Self { source_code, tokens: tokens.peekable(), token: None, span: Span::EMPTY };
        parser.advance();
        parser
    }

    fn parse_word(&mut self) -> Option<ParseResult> {
        self.parse_integer()
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_char())
            .or_else(|| self.parse_invocation())
            .or_else(|| self.parse_lambda())
            .or_else(|| self.parse_array())
            .or_else(|| self.parse_error())
    }

    fn parse_integer(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Number)?;
        let int = i64::from_str(self.lexeme(&self.span)).unwrap();
        let word = self.spanned(Word::Integer(int));
        self.advance();
        Some(Ok(word))
    }

    fn parse_string(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::String)?;
        let mut str = String::from(self.lexeme(&self.span));
        str.pop();
        str.remove(0);
        let word = self.spanned(Word::String(str));
        self.advance();
        Some(Ok(word))
    }

    fn parse_char(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Char)?;
        let c = match self.lexeme(&self.span) {
            r"'\n'" => '\n',
            r"'\r'" => '\r',
            r"'\t'" => '\t',
            r"'\\'" => '\\',
            raw_char => raw_char.chars().nth(1).unwrap()
        };
        let word = self.spanned(Word::Char(c));
        self.advance();
        Some(Ok(word))
    }

    fn parse_invocation(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Identifier)?;
        let identifier = String::from(self.lexeme(&self.span));
        let word = self.spanned(Word::Invocation(identifier));
        self.advance();
        Some(Ok(word))
    }

    fn parse_array(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::OpenBracket)?;
        self.advance();

        match self.parse_series(Self::parse_array_element) {
            Ok(elements) => {
                let result = match self.expect_token(&Token::CloseBracket) {
                    None => Err(self.spanned(ParseError::ExpectedToken(Token::CloseBracket))),
                    Some(_) => {
                        let word = self.spanned(Word::Array(elements));
                        self.advance();
                        Ok(word)
                    }
                };
                Some(result)
            }
            Err(error) => Some(Err(error))
        }
    }

    fn parse_array_element(&mut self) -> Option<ParseResult> {
        self.parse_integer()
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_array())
    }

    fn parse_lambda(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::OpenParenthesis)?;
        self.advance();

        match self.parse_series(Self::parse_lambda_word) {
            Ok(words) => {
                let result = match self.expect_token(&Token::CloseParenthesis) {
                    None => Err(self.spanned(ParseError::ExpectedToken(Token::CloseParenthesis))),
                    Some(_) => {
                        let word = self.spanned(Word::Lambda(words));
                        self.advance();
                        Ok(word)
                    }
                };
                Some(result)
            }
            Err(error) => Some(Err(error))
        }
    }

    fn parse_lambda_word(&mut self) -> Option<ParseResult> {
        self.parse_integer()
            .or_else(|| self.parse_char())
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_invocation())
            .or_else(|| self.parse_array())
    }

    fn parse_error(&mut self) -> Option<ParseResult> {
        match &self.token {
            Some(Ok(token)) => Some(Err(self.spanned(ParseError::UnexpectedToken(token.value.clone())))),
            Some(Err(lex_error)) => Some(Err(self.spanned(Lex(lex_error.value.clone())))),
            None => None
        }
    }

    fn parse_series<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> Option<ParseResult<T>>,
    ) -> ParseResult<Vec<T>>
    {
        let mut elements = Vec::new();

        while let Some(result) = parser(self) {
            elements.push(result?);
        }

        Ok(elements)
    }

    fn advance(&mut self) {
        self.token = self.tokens.next();
        self.update_span();
    }

    fn expect_token(&mut self, expected: &Token) -> Option<()> {
        if let Some(Ok(spanned_token)) = &self.token && &spanned_token.value == expected {
            Some(())
        } else {
            None
        }
    }

    fn update_span(&mut self) {
        match &self.token {
            Some(Err(error)) => self.span = error.span.clone(),
            Some(Ok(token)) => self.span = token.span.clone(),
            _ => {}
        }
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }
}

impl<'a, TTokens> Iterator for Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    type Item = ParseResult<Spanned<Word>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_word()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item=ParseResult<Spanned<Word>>> + '_ {
    let tokens = tokenize(source);
    Parser::new(source, tokens)
}
