use crate::lexer::{tokenize, LexError, LexTokenResult, Token};
use crate::location::{Span, Spanned};
use crate::parser::ParseError::Lex;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
    UnknownIdentifier(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {token}"),
            ParseError::ExpectedToken(token) => write!(f, "Expected token: {token}"),
            ParseError::UnknownIdentifier(identifier) => write!(f, "Unknown identifier: {identifier}"),
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug, PartialEq)]
pub enum Word {
    Char(char),
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
    Niladic(NiladicOperation),
    Monadic(MonadicOperation),
    Dyadic(DyadicOperation),
    Array(Vec<Spanned<Word>>),
    Lambda(Vec<Spanned<Word>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum NiladicOperation {
    Stack
}

impl Display for NiladicOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NiladicOperation::Stack => write!(f, "?")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MonadicOperation {
    Not
}

impl Display for MonadicOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MonadicOperation::Not => write!(f, "!")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DyadicOperation {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Stack,
}

impl Display for DyadicOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DyadicOperation::Add => write!(f, "+"),
            DyadicOperation::Sub => write!(f, "-"),
            DyadicOperation::Mul => write!(f, "*"),
            DyadicOperation::Div => write!(f, "/"),
            DyadicOperation::Equal => write!(f, "="),
            DyadicOperation::NotEqual => write!(f, "!="),
            DyadicOperation::Greater => write!(f, ">"),
            DyadicOperation::GreaterEqual => write!(f, ">="),
            DyadicOperation::Less => write!(f, "<"),
            DyadicOperation::LessEqual => write!(f, "<="),
            DyadicOperation::Stack => write!(f, "?"),
        }
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Word::Integer(i) => write!(f, "{i}"),
            Word::Float(float) => write!(f, "{float}"),
            Word::Char(c) => write!(f, "'{c}'"),
            Word::String(str) => write!(f, "\"{str}\""),
            Word::Identifier(identifier) => write!(f, "{identifier}()"),
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
            Word::Lambda(_) => write!(f, "lambda"),
            Word::Niladic(operation) => write!(f, "{operation}"),
            Word::Monadic(operation) => write!(f, "{operation}"),
            Word::Dyadic(operation) => write!(f, "{operation}"),
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
            .or_else(|| self.parse_float())
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_char())
            .or_else(|| self.parse_operator())
            .or_else(|| self.parse_identifier())
            .or_else(|| self.parse_lambda())
            .or_else(|| self.parse_array())
            .or_else(|| self.parse_error())
    }

    fn parse_integer(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Integer)?;
        let src = self.lexeme(&self.span);

        let int = i64::from_str(src).unwrap();
        let word = self.spanned(Word::Integer(int));

        self.advance();
        Some(Ok(word))
    }

    fn parse_float(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Float)?;
        let src = self.lexeme(&self.span);

        let float = f64::from_str(src).unwrap();
        let word = self.spanned(Word::Float(float));

        self.advance();
        Some(Ok(word))
    }

    fn parse_string(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::String)?;

        let mut str = String::with_capacity(self.span.length as usize);
        let mut chars = self.lexeme(&self.span)[1..(self.span.length - 1) as usize].chars();

        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => str.push('\n'),
                    Some('r') => str.push('\r'),
                    Some('t') => str.push('\t'),
                    Some('\\') => str.push('\\'),
                    Some('\'') => str.push('\''),
                    _ => panic!("invalid escape sequence")
                }
            } else {
                str.push(c);
            }
        }

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
            r"'\''" => '\'',
            raw_char => raw_char.chars().nth(1).unwrap()
        };
        let word = self.spanned(Word::Char(c));
        self.advance();
        Some(Ok(word))
    }
    fn parse_operator(&mut self) -> Option<ParseResult> {
        if let Some(Ok(spanned_token)) = &self.token {
            let word = match spanned_token.value {
                Token::Plus => Word::Dyadic(DyadicOperation::Add),
                Token::Minus => Word::Dyadic(DyadicOperation::Sub),
                Token::Star => Word::Dyadic(DyadicOperation::Mul),
                Token::Slash => Word::Dyadic(DyadicOperation::Div),
                Token::Equal => Word::Dyadic(DyadicOperation::Equal),
                Token::BangEqual => Word::Dyadic(DyadicOperation::NotEqual),
                Token::Bang => Word::Monadic(MonadicOperation::Not),
                Token::Greater => Word::Dyadic(DyadicOperation::Greater),
                Token::GreaterEqual => Word::Dyadic(DyadicOperation::GreaterEqual),
                Token::Less => Word::Dyadic(DyadicOperation::Less),
                Token::LessEqual => Word::Dyadic(DyadicOperation::LessEqual),
                Token::QuestionMark => Word::Niladic(NiladicOperation::Stack),
                _ => return None
            };
            let word = self.spanned(word);
            self.advance();
            Some(Ok(word))
        } else {
            None
        }
    }

    fn parse_identifier(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Identifier)?;

        let identifier = self.lexeme(&self.span).to_string();
        let word = self.spanned(Word::Identifier(identifier));
        self.advance();
        Some(Ok(word))
    }

    fn parse_array(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::OpenBracket)?;

        let start = self.span.clone();
        self.advance();

        match self.parse_series(Self::parse_array_element) {
            Ok(elements) => {
                self.span = self.span.merge(&start);

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
            .or_else(|| self.parse_float())
            .or_else(|| self.parse_char())
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_array())
    }

    fn parse_lambda(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::OpenParenthesis)?;

        let start = self.span.clone();
        self.advance();

        match self.parse_series(Self::parse_lambda_word) {
            Ok(words) => {
                self.span = self.span.merge(&start);

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
            .or_else(|| self.parse_float())
            .or_else(|| self.parse_char())
            .or_else(|| self.parse_string())
            .or_else(|| self.parse_operator())
            .or_else(|| self.parse_identifier())
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

    fn token_matches(&mut self, expected: &Token) -> bool {
        if let Some(Ok(spanned_token)) = &self.token && &spanned_token.value == expected {
            true
        } else {
            false
        }
    }

    fn expect_token(&mut self, expected: &Token) -> Option<()> {
        if self.token_matches(expected) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integers() {
        let mut words = parse("1 23 -456");

        assert_eq!(Some(Ok(Spanned::new(Word::Integer(1), Span::new(0, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Integer(23), Span::new(2, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Integer(-456), Span::new(5, 4)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_floats() {
        let mut words = parse("5.134 -6.1 7.");

        assert_eq!(Some(Ok(Spanned::new(Word::Float(5.134), Span::new(0, 5)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Float(-6.1), Span::new(6, 4)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Float(7.0), Span::new(11, 2)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_strings() {
        let mut words = parse(r#""foo" "a b c" "\n""#);

        assert_eq!(Some(Ok(Spanned::new(Word::String("foo".to_string()), Span::new(0, 5)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::String("a b c".to_string()), Span::new(6, 7)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::String("\n".to_string()), Span::new(14, 4)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_characters() {
        let mut words = parse(r#"'a' '8' '\n' '\''"#);

        assert_eq!(Some(Ok(Spanned::new(Word::Char('a'), Span::new(0, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Char('8'), Span::new(4, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Char('\n'), Span::new(8, 4)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Char('\''), Span::new(13, 4)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_operators() {
        let mut words = parse("+-*/=!!=>>=<<=?");

        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Add), Span::new(0, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Sub), Span::new(1, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Mul), Span::new(2, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Div), Span::new(3, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Equal), Span::new(4, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Monadic(MonadicOperation::Not), Span::new(5, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::NotEqual), Span::new(6, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Greater), Span::new(8, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::GreaterEqual), Span::new(9, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::Less), Span::new(11, 1)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Dyadic(DyadicOperation::LessEqual), Span::new(12, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Niladic(NiladicOperation::Stack), Span::new(14, 1)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_identifiers() {
        let mut words = parse("foo Bar BAZ read-file read_file1 empty? x2");

        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("foo".to_string()), Span::new(0, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("Bar".to_string()), Span::new(4, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("BAZ".to_string()), Span::new(8, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("read-file".to_string()), Span::new(12, 9)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("read_file1".to_string()), Span::new(22, 10)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("empty?".to_string()), Span::new(33, 6)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Identifier("x2".to_string()), Span::new(40, 2)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_arrays() {
        let mut words = parse("[] [1] [23 55] [[1 2] [3 4]]");

        assert_eq!(Some(Ok(Spanned::new(Word::Array(vec![]), Span::new(0, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(1), Span::new(4, 1))]), Span::new(3, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(23), Span::new(8, 2)), Spanned::new(Word::Integer(55), Span::new(11, 2))]), Span::new(7, 7)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Array(vec![Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(1), Span::new(17, 1)), Spanned::new(Word::Integer(2), Span::new(19, 1))]), Span::new(16, 5)), Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(3), Span::new(23, 1)), Spanned::new(Word::Integer(4), Span::new(25, 1))]), Span::new(22, 5))]), Span::new(15, 13)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_lambdas() {
        let mut words = parse("() (1) (2 swap dup)");

        assert_eq!(Some(Ok(Spanned::new(Word::Lambda(vec![]), Span::new(0, 2)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Lambda(vec![Spanned::new(Word::Integer(1), Span::new(4, 1))]), Span::new(3, 3)))), words.next());
        assert_eq!(Some(Ok(Spanned::new(Word::Lambda(vec![Spanned::new(Word::Integer(2), Span::new(8, 1)), Spanned::new(Word::Identifier("swap".to_string()), Span::new(10, 4)), Spanned::new(Word::Identifier("dup".to_string()), Span::new(15, 3))]), Span::new(7, 12)))), words.next());
        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_ignores_whitespace() {
        let mut words = parse(" \r\n\t \n");

        assert_eq!(None, words.next())
    }

    #[test]
    fn test_parse_ignores_comments() {
        let mut words = parse("# first comment \r\n\t # 2nd comment\n#comment 3");

        assert_eq!(None, words.next())
    }

    // TODO: add tests for error conditions
}
