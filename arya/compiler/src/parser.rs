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
    NonScalarArrayElement,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::Unexpected(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::Expected(token) => write!(f, "Expected token: {:?}", token),
            ParseError::UnknownIdentifier(name) => write!(f, "Unknown identifier '{name}'"),
            ParseError::IrregularMatrix => write!(f, "Not all rows of the matrix have the same length"),
            ParseError::NonScalarArrayElement => write!(f, "Array contains a non-scalar element"),
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
    Integer(i64),
    Primitive(Primitive),
    Array(Vec<Spanned<Word>>),
    Lambda(Lambda),
}

impl Word {
    pub fn signature(&self) -> Signature {
        match self {
            Word::Integer(_) |
            Word::Array(_) => Signature::new(0, 1),
            Word::Primitive(primitive) => primitive.signature(),
            Word::Lambda(lambda) => lambda.to_owned().signature,
        }
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Word::Integer(i) => write!(f, "{i}"),
            Word::Array(array) => {
                write!(f, "[")?;
                for (i, element) in array.iter().enumerate() {
                    write!(f, "{}", element.value)?;
                    if i < array.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
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
    (2, 1, Reduce),    
);
// TODO: fold
// TODO: fork
// TODO: bracket
// TODO: both

pub type ParseWordResult = ParseResult<Spanned<Word>>;
type ParseResult<T> = Result<T, Spanned<ParseError>>;

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
        let mut tokens = tokens.peekable();
        let token = tokens.next();

        Parser { source_code, tokens, token, span: Span::EMPTY }
    }

    fn try_parse_word(&mut self) -> Option<ParseWordResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_array())
            .or_else(|| self.try_parse_lambda())
            .or_else(|| self.try_parse_error())
    }

    fn try_parse_integer(&mut self) -> Option<ParseWordResult> {
        self.advance_map_if_token(&Token::Number, Self::parse_integer)
    }

    fn parse_integer(&mut self) -> ParseWordResult {
        let int = i64::from_str(self.lexeme(&self.span)).unwrap();
        Ok(self.make_word(Word::Integer(int)))
    }

    fn try_parse_primitive(&mut self) -> Option<ParseWordResult> {
        self.advance_map_if_token(&Token::Plus, |parser| parser.make_primitive(Primitive::Add))
            .or_else(|| self.advance_map_if_token(&Token::Minus, |parser| parser.make_primitive(Primitive::Subtract)))
            .or_else(|| self.advance_map_if_token(&Token::Star, |parser| parser.make_primitive(Primitive::Multiply)))
            .or_else(|| self.advance_map_if_token(&Token::Slash, |parser| parser.make_primitive(Primitive::Divide)))
            .or_else(|| self.advance_map_if_token(&Token::Ampersand, |parser| parser.make_primitive(Primitive::And)))
            .or_else(|| self.advance_map_if_token(&Token::Pipe, |parser| parser.make_primitive(Primitive::Or)))
            .or_else(|| self.advance_map_if_token(&Token::Caret, |parser| parser.make_primitive(Primitive::Xor)))
            .or_else(|| self.advance_map_if_token(&Token::Bang, |parser| parser.make_primitive(Primitive::Not)))
            .or_else(|| self.advance_map_if_token(&Token::Underscore, |parser| parser.make_primitive(Primitive::Negate)))
            .or_else(|| self.advance_map_if_token(&Token::Equal, |parser| parser.make_primitive(Primitive::Equal)))
            .or_else(|| self.advance_map_if_token(&Token::NotEqual, |parser| parser.make_primitive(Primitive::NotEqual)))
            .or_else(|| self.advance_map_if_token(&Token::Greater, |parser| parser.make_primitive(Primitive::Greater)))
            .or_else(|| self.advance_map_if_token(&Token::GreaterEqual, |parser| parser.make_primitive(Primitive::GreaterEqual)))
            .or_else(|| self.advance_map_if_token(&Token::Less, |parser| parser.make_primitive(Primitive::Less)))
            .or_else(|| self.advance_map_if_token(&Token::LessEqual, |parser| parser.make_primitive(Primitive::LessEqual)))
            .or_else(|| self.advance_map_if_token(&Token::Dup, |parser| parser.make_primitive(Primitive::Dup)))
            .or_else(|| self.advance_map_if_token(&Token::Drop, |parser| parser.make_primitive(Primitive::Drop)))
            .or_else(|| self.advance_map_if_token(&Token::Swap, |parser| parser.make_primitive(Primitive::Swap)))
            .or_else(|| self.advance_map_if_token(&Token::Over, |parser| parser.make_primitive(Primitive::Over)))
            .or_else(|| self.advance_map_if_token(&Token::Reduce, |parser| parser.make_primitive(Primitive::Reduce)))
    }

    fn make_primitive(&self, primitive: Primitive) -> ParseWordResult {
        Ok(self.make_word(Word::Primitive(primitive)))
    }

    fn try_parse_array(&mut self) -> Option<ParseWordResult> {
        self.advance_map_if_token(&Token::OpenBracket, Self::parse_array)
    }

    fn parse_array(&mut self) -> ParseWordResult {
        let words = self.parse_series(Self::try_parse_array_element)?;
        self.advance_map_if_token(&Token::CloseBracket, |parser| Ok(parser.make_word(Word::Array(words))))
            .unwrap_or_else(|| Err(self.make_error(ParseError::Expected(Token::CloseBracket))))
    }

    fn try_parse_array_element(&mut self) -> Option<ParseWordResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_array())
    }

    fn try_parse_lambda(&mut self) -> Option<ParseWordResult> {
        self.advance_map_if_token(&Token::OpenParenthesis, Self::parse_lambda)
    }

    fn parse_lambda(&mut self) -> ParseWordResult {
        let words = self.parse_series(Self::try_parse_lambda_word)?;
        let signature = Signature::from_words(&words);
        self.advance_map_if_token(&Token::CloseParenthesis, |parser| parser.make_word(Word::Lambda(Lambda::new(signature, words))))
            .ok_or_else(|| self.make_error(ParseError::Expected(Token::CloseParenthesis)))
    }

    fn try_parse_lambda_word(&mut self) -> Option<ParseWordResult> {
        self.try_parse_integer()
            .or_else(|| self.try_parse_primitive())
            .or_else(|| self.try_parse_array())
    }

    fn try_parse_error(&mut self) -> Option<ParseWordResult> {
        match &self.token {
            Some(Ok(token)) => Some(Err(self.make_error(ParseError::Unexpected(token.value.clone())))),
            Some(Err(lex_error)) => Some(Err(self.make_error(Lex(lex_error.value.clone())))),
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
        self.token = self.tokens.next()
            .inspect(|_| self.update_span());
    }

    fn advance_map_if_token<T>(&mut self, expected: &Token, parse: impl FnOnce(&mut Self) -> T) -> Option<T> {
        if let Some(Ok(spanned_token)) = &self.token && &spanned_token.value == expected {
            self.advance();
            Some(parse(self))
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

    fn make_word(&self, word: Word) -> Spanned<Word> {
        self.spanned(word)
    }

    fn make_error(&mut self, error: ParseError) -> Spanned<ParseError> {
        self.spanned(error)
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
    type Item = ParseWordResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.try_parse_word()
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
            Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(7), Span::EMPTY)]), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Add), Span::new(4, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(1, 1));
    }

    #[test]
    fn test_signature_from_words_requiring_extra_inputs() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(1, 1)),
            Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(7), Span::EMPTY)]), Span::new(2, 1)),
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
            Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(1), Span::EMPTY)]), Span::new(2, 1)),
            Spanned::new(Word::Primitive(Primitive::Dup), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Negate), Span::new(4, 1)),
            Spanned::new(Word::Array(vec![Spanned::new(Word::Integer(7), Span::EMPTY)]), Span::new(5, 1)),
            Spanned::new(Word::Primitive(Primitive::Swap), Span::new(6, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(2, 4));
    }
}
