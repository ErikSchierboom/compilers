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
    Integer(i64),
    Array(Vec<Spanned<Word>>),
    Matrix(Vec<Vec<Spanned<Word>>>),
    Primitive(Primitive),
    Lambda(Lambda),
}

impl Word {
    pub fn signature(&self) -> Signature {
        match self {
            Word::Integer(_) |
            Word::Array(_) |
            Word::Matrix(_) => Signature::new(0, 1),
            Word::Primitive(primitive) => primitive.signature(),
            Word::Lambda(lambda) => lambda.to_owned().signature,
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
        let word = if let Some(integer) = self.parse_integer() {
            integer
        } else if let Some(primitive) = self.parse_primitive() {
            primitive
        } else {
            todo!()
        };
        Some(word)


        // let parse_word_result = match self.next_token()? {
        //     Err(lex_error) => self.make_error(Lex(lex_error.value.clone())),
        //     Ok(token) => match &token.value {
        //         Token::Identifier => self.parse_identifier(),
        //         Token::Number => self.parse_integer(),
        //         Token::OpenBracket => self.parse_array(),
        //         Token::OpenParenthesis => self.parse_lambda(),
        //         _ => self.make_error(ParseError::Unexpected(token.value.clone())),
        //     },
        // };
        // Some(parse_word_result)
    }

    fn parse_integer(&mut self) -> Option<ParseWordResult> {
        self.next_if_token_is(&Token::Number)?;
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        Some(self.make_word(Word::Integer(number)))
    }

    fn parse_identifier(&mut self) -> Option<ParseWordResult> {
        self.next_if_token_is(&Token::Identifier)?;
        match self.lexeme(&self.span) {
            "dup" => self.parse_primitive(),
            "drop" => self.parse_primitive(),
            "swap" => self.parse_primitive(),
            "over" => self.parse_primitive(),
            "reduce" => self.parse_primitive(),
            name => Some(self.make_error(ParseError::UnknownIdentifier(name.to_string())))
        }
    }

    fn parse_primitive(&mut self) -> Option<ParseWordResult> {
        if self.next_if_token_is(&Token::Plus).is_some() {
            Some(self.make_word(Word::Primitive(Primitive::Add)))
        } else {
            None
        }
    }

    fn parse_array(&mut self) -> Option<ParseWordResult> {
        match self.parse_delimited(|parser| parser.parse_integer(), Token::Semicolon) {
            Ok(elements) => {
                match elements.len() {
                    0 => Some(self.make_word(Word::Array(vec![]))),
                    1 => {
                        Some(self.make_word(Word::Array(elements.first().unwrap().to_owned())))
                    }
                    _ => {
                        if elements.windows(2).all(|pair| pair[0].len() == pair[1].len()) {
                            Some(self.make_word(Word::Matrix(elements)))
                        } else {
                            Some(self.make_error(ParseError::IrregularMatrix))
                        }
                    }
                }
            }
            Err(error) => Some(Err(error))
        }
    }

    fn parse_lambda(&mut self) -> ParseWordResult {
        self.parse_until(Token::CloseParenthesis, |words| {
            let signature = Signature::from_words(&words);
            Word::Lambda(Lambda::new(signature, words))
        })
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

    fn parse_until(
        &mut self,
        closing_token: Token,
        to_word: impl Fn(Vec<Spanned<Word>>) -> Word,
    ) -> ParseWordResult
    {
        let start_span = self.span.clone();
        let mut elements = Vec::new();

        loop {
            match self.peek() {
                None => return self.make_error(ParseError::Expected(closing_token)),
                Some(Err(lex_error)) => {
                    let error = lex_error.value.clone();
                    self.next_token();
                    return self.make_error(Lex(error));
                }
                Some(Ok(token)) if token.value == closing_token => {
                    self.next_token();
                    self.span = start_span.merge(&self.span);
                    return self.make_word(to_word(elements));
                }
                Some(Ok(_)) => match self.parse_word() {
                    None => return self.make_error(ParseError::Expected(closing_token)),
                    Some(Err(error)) => return self.make_error(error.value),
                    Some(Ok(element)) => elements.push(element),
                },
            }
        }
    }

    fn make_word(&self, word: Word) -> ParseWordResult {
        Ok(self.spanned(word))
    }

    fn make_error(&mut self, error: ParseError) -> ParseWordResult {
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
            Spanned::new(Word::Integer(1), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Add), Span::new(4, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(1, 1));
    }

    #[test]
    fn test_signature_from_words_requiring_extra_inputs() {
        let words: Vec<Spanned<Word>> = vec![
            Spanned::new(Word::Primitive(Primitive::Multiply), Span::new(1, 1)),
            Spanned::new(Word::Integer(1), Span::new(2, 1)),
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
            Spanned::new(Word::Integer(1), Span::new(2, 1)),
            Spanned::new(Word::Primitive(Primitive::Dup), Span::new(3, 1)),
            Spanned::new(Word::Primitive(Primitive::Negate), Span::new(4, 1)),
            Spanned::new(Word::Integer(7), Span::new(5, 1)),
            Spanned::new(Word::Primitive(Primitive::Swap), Span::new(6, 1)),
        ];
        let signature = Signature::from_words(&words);
        assert_eq!(signature, Signature::new(2, 4));
    }
}
