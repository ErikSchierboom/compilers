use crate::array::{Array, Scalar};
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
    Unexpected(Token),
    Expected(Token),
    UnknownIdentifier(String),
    IrregularMatrix,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::Unexpected(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::Expected(token) => write!(f, "Expected token: {:?}", token),
            ParseError::UnknownIdentifier(name) => write!(f, "Unknown identifier '{name}'"),
            ParseError::IrregularMatrix => write!(f, "Not all rows of the matrix have the same length"),
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug, PartialEq)]
pub struct Signature {
    pub num_inputs: u8,
    pub num_outputs: u8,
}

impl Signature {
    pub const EMPTY: Self = Self {
        num_inputs: 0,
        num_outputs: 0,
    };

    pub fn new(num_inputs: u8, num_outputs: u8) -> Self {
        Self {
            num_inputs,
            num_outputs,
        }
    }

    pub fn merge(&self, other: &Self) -> Self {
        let required_inputs = self.num_inputs + (other.num_inputs as i8 - self.num_outputs as i8).max(0) as u8;
        let produced_outputs = (self.num_outputs as i8 - other.num_inputs as i8).max(0) as u8 + other.num_outputs;
        Self::new(required_inputs, produced_outputs)
    }

    pub fn from_words(words: &Vec<Spanned<Word>>) -> Self {
        words.iter().fold(Signature::EMPTY, |acc, word| acc.merge(&word.value.signature()))
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    Array(Array),
    Primitive(Primitive),
    Lambda(Lambda),
}

impl Word {
    pub fn signature(&self) -> Signature {
        match self {
            Word::Array(_) => Signature::new(0, 1),
            Word::Primitive(primitive) => primitive.signature(),
            Word::Lambda(lambda) => lambda.to_owned().signature,
        }
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Word::Array(array) => write!(f, "{array}"),
            Word::Primitive(primitive) => write!(f, "{primitive}"),
            Word::Lambda(lambda) => write!(f, "{lambda}"),
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
        write!(f, "TODO: lambda display")
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
                    $( Primitive::$name => write!(f, "$name"), )*
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
    (2, 1, Reduce),    
);
// TODO: fold
// TODO: fork
// TODO: bracket
// TODO: both

pub type ParseWordResult = ParseResult<Spanned<Word>>;
type ParseScalarResult = ParseResult<Spanned<Scalar>>;
type ParseResult<T> = Result<T, Spanned<ParseError>>;

struct Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    source_code: &'a str,
    tokens: Peekable<TTokens>,
    span: Span,
}

impl<'a, TTokens> Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    fn new(source_code: &'a str, tokens: TTokens) -> Self {
        Parser {
            source_code,
            tokens: tokens.peekable(),
            span: Span::EMPTY,
        }
    }

    fn parse_word(&mut self) -> Option<ParseWordResult> {
        let parse_word_result = match self.next_token()? {
            Err(lex_error) => self.make_error(Lex(lex_error.value.clone())),
            Ok(token) => match &token.value {
                Token::Identifier => self.parse_identifier(),
                Token::Number => self.parse_integer(),
                Token::OpenBracket => self.parse_array(),
                Token::OpenParenthesis => self.parse_lambda(),
                Token::Plus => self.parse_primitive(Primitive::Add),
                Token::Minus => self.parse_primitive(Primitive::Subtract),
                Token::Star => self.parse_primitive(Primitive::Multiply),
                Token::Slash => self.parse_primitive(Primitive::Divide),
                Token::Ampersand => self.parse_primitive(Primitive::And),
                Token::Pipe => self.parse_primitive(Primitive::Or),
                Token::Caret => self.parse_primitive(Primitive::Xor),
                Token::Bang => self.parse_primitive(Primitive::Not),
                Token::Underscore => self.parse_primitive(Primitive::Negate),
                Token::Equal => self.parse_primitive(Primitive::Equal),
                Token::NotEqual => self.parse_primitive(Primitive::NotEqual),
                Token::Greater => self.parse_primitive(Primitive::Greater),
                Token::GreaterEqual => self.parse_primitive(Primitive::GreaterEqual),
                Token::Less => self.parse_primitive(Primitive::Less),
                Token::LessEqual => self.parse_primitive(Primitive::LessEqual),
                _ => self.make_error(ParseError::Unexpected(token.value.clone())),
            },
        };
        Some(parse_word_result)
    }

    fn parse_integer(&mut self) -> ParseWordResult {
        self.parse_scalar_integer().and_then(|scalar| self.make_word(Word::Array(Array::scalar(scalar))))
    }

    fn parse_scalar_integer(&mut self) -> ParseScalarResult {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.make_scalar(Scalar::Integer(number))
    }

    fn try_parse_scalar_integer(&mut self) -> Option<ParseScalarResult> {
        self.next_if_token_is(&Token::Number).map(|_| self.parse_scalar_integer())
    }

    fn try_parse_scalar(&mut self) -> Option<ParseScalarResult> {
        self.try_parse_scalar_integer()
    }

    fn parse_identifier(&mut self) -> ParseWordResult {
        match self.lexeme(&self.span) {
            "dup" => self.parse_primitive(Primitive::Dup),
            "drop" => self.parse_primitive(Primitive::Drop),
            "swap" => self.parse_primitive(Primitive::Swap),
            "over" => self.parse_primitive(Primitive::Over),
            "reduce" => self.parse_primitive(Primitive::Reduce),
            name => self.make_error(ParseError::UnknownIdentifier(name.to_string()))
        }
    }

    fn parse_primitive(&self, primitive: Primitive) -> ParseWordResult {
        self.make_word(Word::Primitive(primitive))
    }

    fn parse_array(&mut self) -> ParseWordResult {
        let elements = self.parse_until(|s| s.parse_array_elements(), Token::CloseBracket)?;

        match elements.len() {
            0 => self.make_word(Word::Array(Array::linear(Vec::new()))),
            1 => {
                self.make_word(Word::Array(Array::linear(elements.first().unwrap().to_owned())))
            }
            _ => {
                if elements.windows(2).all(|pair| pair[0].len() == pair[1].len()) {
                    self.make_word(Word::Array(Array::matrix(elements)))
                } else {
                    self.make_error(ParseError::IrregularMatrix)
                }
            }
        }
    }

    fn parse_array_elements(&mut self) -> ParseResult<Vec<Vec<Spanned<Scalar>>>> {
        self.parse_delimited(|parser| parser.try_parse_scalar(), Token::Semicolon)
    }

    fn parse_lambda(&mut self) -> ParseWordResult {
        todo!("parse lambda")
        // self.parse_until(Token::CloseParenthesis, |words| {
        //     let signature = Signature::from_words(&words);
        //     Word::Lambda(Lambda::new(signature, words))
        // })
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

    fn parse_delimited<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> Option<ParseResult<T>>,
        delimiter: Token,
    ) -> ParseResult<Vec<Vec<T>>>
    {
        let mut delimited_elements = Vec::new();

        loop {
            let series = self.parse_series(&parser)?;
            delimited_elements.push(series);
            if let None = self.next_if_token_is(&delimiter) {
                break;
            }
        }

        Ok(delimited_elements)
    }

    fn parse_until<T>(
        &mut self,
        parser: impl Fn(&mut Self) -> ParseResult<T>,
        close_token: Token,
    ) -> ParseResult<T>
    {
        let result = parser(self)?;
        match self.next_if_token_is(&Token::CloseBracket) {
            None => self.make_error(ParseError::Expected(close_token)),
            Some(_) => Ok(result)
        }
    }

    fn make_scalar(&self, scalar: Scalar) -> ParseScalarResult {
        Ok(self.spanned(scalar))
    }

    fn make_word(&self, word: Word) -> ParseWordResult {
        Ok(self.spanned(word))
    }

    fn make_error<T>(&mut self, error: ParseError) -> ParseResult<T> {
        Err(self.spanned(error))
    }

    fn lexeme(&self, span: &Span) -> &'a str {
        &self.source_code[span.position as usize..(span.position + span.length as u32) as usize]
    }

    fn peek(&mut self) -> Option<&LexTokenResult> {
        self.tokens.peek()
    }

    fn next_token(&mut self) -> Option<LexTokenResult> {
        self.next_token_if(|_| true)
    }

    fn next_if_token_is(&mut self, token: &Token) -> Option<LexTokenResult> {
        self.next_token_if(|spanned_token| spanned_token.value == *token)
    }

    fn next_token_if(&mut self, predicate: impl FnOnce(&Spanned<Token>) -> bool) -> Option<LexTokenResult> {
        self.tokens
            .next_if(|lex_result| lex_result.as_ref().map(predicate).unwrap_or_default())
            .inspect(|lex_result| self.update_span(lex_result))
    }

    fn update_span(&mut self, token_result: &LexTokenResult) {
        match token_result {
            Err(error) => self.span = error.span.clone(),
            Ok(token) => self.span = token.span.clone(),
        }
    }

    fn spanned<V>(&self, value: V) -> Spanned<V> {
        Spanned::new(value, self.span.clone())
    }
}

impl<'a, TTokens> Iterator for Parser<'a, TTokens>
where
    TTokens: Iterator<Item=LexTokenResult>,
{
    type Item = ParseWordResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_word()
    }
}

pub fn parse(source: &str) -> impl Iterator<Item=ParseWordResult> + '_ {
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
            Spanned::new(Word::Array(Array::scalar(Spanned::new(Scalar::Integer(1), Span::EMPTY))), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Add), Span::new(4, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(1, 1));
    }

    #[test]
    fn test_signature_from_words_requiring_extra_inputs() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(1, 1)),
            Spanned::new(Word::Array(Array::scalar(Spanned::new(Scalar::Integer(1), Span::EMPTY))), Span::new(2, 1)),
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
            Spanned::new(Word::Array(Array::scalar(Spanned::new(Scalar::Integer(1), Span::EMPTY))), Span::new(2, 1)),
            Spanned::new(Word::Primitive(Primitive::Dup), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Negate), Span::new(4, 1)),
            Spanned::new(Word::Array(Array::scalar(Spanned::new(Scalar::Integer(7), Span::EMPTY))), Span::new(5, 1)),
            Spanned::new(Word::Primitive(Primitive::Swap), Span::new(6, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(2, 4));
    }
}
