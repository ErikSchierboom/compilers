use crate::array::Array;
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
    ExpectedModifier,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::UnexpectedToken(token) => write!(f, "Unexpected token: {token}"),
            ParseError::ExpectedToken(token) => write!(f, "Expected token: {token}"),
            ParseError::ExpectedModifier => write!(f, "Expected modifier function")
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Signature {
    pub input_count: u8,
    pub output_count: u8,
}

impl Signature {
    pub const EMPTY: Self = Self {
        input_count: 0,
        output_count: 0,
    };

    pub fn new(input_count: u8, output_count: u8) -> Self {
        Self {
            input_count,
            output_count,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        let required_inputs = self.input_count + (other.input_count as i8 - self.output_count as i8).max(0) as u8;
        let produced_outputs = (self.output_count as i8 - other.input_count as i8).max(0) as u8 + other.output_count;
        Self::new(required_inputs, produced_outputs)
    }

    pub fn from_words(words: &Vec<Spanned<Word>>) -> Self {
        words.iter().fold(Signature::EMPTY, |acc, word| acc.merge(&word.value.signature()))
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    String(String),
    Char(char),
    Integer(i64),
    Primitive(Primitive),
    Modifier(Modifier),
    Array(Array<Spanned<Word>>),
}

impl Word {
    pub fn signature(&self) -> Signature {
        match self {
            Word::Primitive(primitive) => primitive.signature(),
            Word::Modifier(modifier) => modifier.signature(),
            _ => Signature::new(0, 1),
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Word::Integer(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<Array<Spanned<Word>>> {
        match self {
            Word::Array(array) => Some(array.clone()),
            _ => None,
        }
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Word::Integer(i) => write!(f, "{i}"),
            Word::Char(c) => write!(f, "'{c}'"),
            Word::String(str) => write!(f, "\"{str}\""),
            Word::Array(array) => {
                write!(f, "[")?;
                for (i, element) in array.values.iter().enumerate() {
                    write!(f, "{}", element.value)?;
                    if i < array.values.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            Word::Primitive(primitive) => write!(f, "{primitive}"),
            Word::Modifier(modifier) => write!(f, "{modifier}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub signature: Signature,
    pub body: Vec<Spanned<Word>>,
}

impl Lambda {
    pub fn new(signature: Signature, body: Vec<Spanned<Word>>) -> Self {
        Self { signature, body }
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda)")
    }
}

macro_rules! primitive {
    ($( ($num_inputs:expr, $num_outputs:expr, $name:ident) ),* $(,)?) => {
        #[derive(Clone, Debug)]
        pub enum Primitive {
            $($name),*
        }

        impl Primitive {
            pub fn signature(&self) -> Signature {
                match self {
                    $( Primitive::$name => Signature::new($num_inputs, $num_outputs), )*
                }
            }
        }

        impl Display for Primitive {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( Primitive::$name => write!(f, stringify!($name)), )*
                }
            }
        }
    };
}

// Defines an enum for built-in primitives using the format: (num_inputs, num_outputs, name)
primitive!(
    (2, 1, Add),
    (2, 1, Subtract),
    (2, 1, Multiply),
    (2, 1, Divide),
    (2, 1, Xor),
    (2, 1, And),
    (2, 1, Or),
    (2, 1, Not),
    (1, 1, Negate),
    (2, 1, Equal),
    (2, 1, NotEqual),
    (2, 1, Greater),
    (2, 1, GreaterEqual),
    (2, 1, Less),
    (2, 1, LessEqual),
    (1, 2, Dup),
    (1, 0, Drop),
    (2, 2, Swap),
    (2, 3, Over),
    (1, 1, Reverse),
    (2, 1, Keep),
    (0, 0, Stack),
    (2, 1, Max),
    (2, 1, Min),
);

macro_rules! modifier {
    ($( ($num_inputs:expr, $num_outputs:expr, $name:ident) ),* $(,)?) => {
        #[derive(Clone, Debug)]
        pub enum Modifier {
            $($name(Spanned<Lambda>)),*
        }

        impl Modifier {
            pub fn signature(&self) -> Signature {
                match self {
                    $( Modifier::$name(_) => Signature::new($num_inputs, $num_outputs), )*
                }
            }

            pub fn lambda(&self) -> &Lambda {
                match self {
                    $( Modifier::$name(spanned_lambda) => &spanned_lambda.value, )*
                }
            }
        }

        impl Display for Modifier {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                match self {
                    $( Modifier::$name(_) => write!(f, stringify!($name)), )*
                }
            }
        }
    };
}

modifier!(
    (2, 1, Reduce),
    (3, 1, Fold),
    (3, 1, Both),
);

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

    fn try_parse_word(&mut self) -> Option<ParseResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_string())
            .or_else(|| self.try_parse_char())
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_modifier())
            .or_else(|| self.try_parse_array())
            .or_else(|| self.try_parse_error())
    }

    fn try_parse_integer(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::Number)?;
        let int = i64::from_str(self.lexeme(&self.span)).unwrap();
        let word = self.spanned(Word::Integer(int));
        self.advance();
        Some(Ok(word))
    }

    fn try_parse_string(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::String)?;
        let mut str = String::from(self.lexeme(&self.span));
        str.pop();
        str.remove(0);
        let word = self.spanned(Word::String(str));
        self.advance();
        Some(Ok(word))
    }

    fn try_parse_char(&mut self) -> Option<ParseResult> {
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

    fn try_parse_primitive(&mut self) -> Option<ParseResult> {
        if let Some(Ok(spanned_token)) = &self.token {
            let primitive = match spanned_token.value {
                Token::Plus => Primitive::Add,
                Token::Minus => Primitive::Subtract,
                Token::Star => Primitive::Multiply,
                Token::Slash => Primitive::Divide,
                Token::Ampersand => Primitive::And,
                Token::Pipe => Primitive::Or,
                Token::Caret => Primitive::Xor,
                Token::Bang => Primitive::Not,
                Token::Underscore => Primitive::Negate,
                Token::Equal => Primitive::Equal,
                Token::NotEqual => Primitive::NotEqual,
                Token::Greater => Primitive::Greater,
                Token::GreaterEqual => Primitive::GreaterEqual,
                Token::Less => Primitive::Less,
                Token::LessEqual => Primitive::LessEqual,
                Token::Dup => Primitive::Dup,
                Token::Drop => Primitive::Drop,
                Token::Swap => Primitive::Swap,
                Token::Over => Primitive::Over,
                Token::Reverse => Primitive::Reverse,
                Token::Keep => Primitive::Keep,
                Token::QuestionMark => Primitive::Stack,
                _ => return None
            };

            let word = self.spanned(Word::Primitive(primitive));
            self.advance();
            Some(Ok(word))
        } else {
            None
        }
    }

    fn try_parse_modifier(&mut self) -> Option<ParseResult> {
        match self.try_parse_lambda()? {
            Ok(spanned_lambda) => {
                if let Some(Ok(spanned_token)) = &self.token {
                    let modifier = match spanned_token.value {
                        Token::Reduce => Modifier::Reduce(spanned_lambda),
                        Token::Fold => Modifier::Fold(spanned_lambda),
                        Token::Both => Modifier::Both(spanned_lambda),
                        _ => return Some(Err(self.spanned(ParseError::ExpectedModifier)))
                    };

                    let word = self.spanned(Word::Modifier(modifier));
                    self.advance();
                    Some(Ok(word))
                } else {
                    Some(Err(self.spanned(ParseError::ExpectedModifier)))
                }
            }
            Err(err) => Some(Err(err))
        }
    }

    fn try_parse_array(&mut self) -> Option<ParseResult> {
        self.expect_token(&Token::OpenBracket)?;
        self.advance();

        match self.parse_series(Self::try_parse_array_element) {
            Ok(elements) => {
                let array = Array::linear(elements);
                let result = match self.expect_token(&Token::CloseBracket) {
                    None => Err(self.spanned(ParseError::ExpectedToken(Token::CloseBracket))),
                    Some(_) => {
                        self.advance();
                        Ok(self.spanned(Word::Array(array)))
                    }
                };
                Some(result)
            }
            Err(error) => Some(Err(error))
        }
    }

    fn try_parse_array_element(&mut self) -> Option<ParseResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_array())
    }

    fn try_parse_lambda(&mut self) -> Option<ParseResult<Spanned<Lambda>>> {
        self.expect_token(&Token::OpenParenthesis)?;
        self.advance();

        match self.parse_series(Self::try_parse_lambda_word) {
            Ok(words) => {
                let signature = Signature::from_words(&words);

                let result = match self.expect_token(&Token::CloseParenthesis) {
                    None => Err(self.spanned(ParseError::ExpectedToken(Token::CloseParenthesis))),
                    Some(_) => {
                        let word = self.spanned(Lambda::new(signature, words));
                        self.advance();
                        Ok(word)
                    }
                };
                Some(result)
            }
            Err(error) => Some(Err(error))
        }
    }

    fn try_parse_lambda_word(&mut self) -> Option<ParseResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_array())
    }

    fn try_parse_error(&mut self) -> Option<ParseResult> {
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
        self.try_parse_word()
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
    fn test_merge_empty_signatures() {
        let lhs = Signature::EMPTY;
        let rhs = Signature::EMPTY;
        let signature = lhs.merge(&rhs);
        assert_eq!(signature, Signature::EMPTY);
    }

    #[test]
    fn test_merge_signatures_with_perfect_overlap() {
        let lhs = Signature::new(1, 2);
        let rhs = Signature::new(2, 1);
        let signature = lhs.merge(&rhs);
        assert_eq!(signature, Signature::new(1, 1));
    }

    #[test]
    fn test_merge_signatures_requiring_extra_inputs() {
        let lhs = Signature::new(1, 2);
        let rhs = Signature::new(3, 2);
        let signature = lhs.merge(&rhs);
        assert_eq!(signature, Signature::new(2, 2));
    }

    #[test]
    fn test_merge_signatures_producing_extra_outputs() {
        let lhs = Signature::new(1, 1);
        let rhs = Signature::new(1, 2);
        let signature = lhs.merge(&rhs);
        assert_eq!(signature, Signature::new(1, 2));
    }

    #[test]
    fn test_merge_signatures_producing_less_outputs() {
        let lhs = Signature::new(1, 2);
        let rhs = Signature::new(2, 0);
        let signature = lhs.merge(&rhs);
        assert_eq!(signature, Signature::new(1, 0));
    }

    #[test]
    fn test_signature_from_words_with_perfect_overlap() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Dup), Span::new(1, 1)),
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(2, 1)),
            Spanned::new(Word::Array(Array::linear(vec![Spanned::new(Word::Integer(7), Span::EMPTY)])), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Add), Span::new(4, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(1, 1));
    }

    #[test]
    fn test_signature_from_words_requiring_extra_inputs() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(1, 1)),
            Spanned::new(Word::Array(Array::linear(vec![Spanned::new(Word::Integer(7), Span::EMPTY)])), Span::new(2, 1)),
            Spanned::new(Word::Primitive(Primitive::Add), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Divide), Span::new(4, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(3, 1));
    }

    #[test]
    fn test_signature_from_words_producing_extra_outputs() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(1, 1)),
            Spanned::new(Word::Array(Array::linear(vec![Spanned::new(Word::Integer(1), Span::EMPTY)])), Span::new(2, 1)),
            Spanned::new(Word::Primitive(Primitive::Dup), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Negate), Span::new(4, 1)),
            Spanned::new(Word::Array(Array::linear(vec![Spanned::new(Word::Integer(7), Span::EMPTY)])), Span::new(5, 1)),
            Spanned::new(Word::Primitive(Primitive::Swap), Span::new(6, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(2, 4));
    }
}
