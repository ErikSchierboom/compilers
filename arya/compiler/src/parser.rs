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
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Lex(lex_error) => write!(f, "{lex_error}"),
            ParseError::Unexpected(token) => write!(f, "Unexpected token: {:?}", token),
            ParseError::Expected(token) => write!(f, "Expected token: {:?}", token),
        }
    }
}

impl Error for ParseError {}

#[derive(Clone, Debug)]
pub struct Signature {
    pub num_arguments: u8,
    pub num_outputs: u8,
}

impl Signature {
    pub fn new(num_arguments: u8, num_outputs: u8) -> Self {
        Self {
            num_arguments,
            num_outputs,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Word {
    Integer(i64),
    Symbol(String),
    Array(Vec<Spanned<Word>>),
    Function(Box<Function>),
}

impl Word {
    pub fn signature(&self) -> Signature {
        match self {
            Word::Integer(_) => Signature::new(0, 1),
            Word::Array(_) => Signature::new(0, 1),
            Word::Symbol(_) => Signature::new(0, 1),
            Word::Function(function) => function.to_owned().signature(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Anonymous(AnonymousFunction),
    Primitive(PrimitiveFunction),
}

#[derive(Clone, Debug)]
pub struct AnonymousFunction {
    pub signature: Signature,
    pub body: Vec<Word>,
}

macro_rules! primitive {
    ($( ($num_inputs:expr, $num_outputs:expr, $name:ident) ),* $(,)?) => {
        #[derive(Clone, Debug)]
        pub enum PrimitiveFunction {
            $($name),*
        }

        impl PrimitiveFunction {
            pub fn signature(&self) -> Signature {
                match self {
                    $( PrimitiveFunction::$name => Signature::new($num_inputs, $num_outputs), )*
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
);

impl Function {
    pub fn signature(self) -> Signature {
        match self {
            Function::Anonymous(anonymous) => anonymous.signature,
            Function::Primitive(primitive) => primitive.signature(),
        }
    }
}

pub type ParseWordResult = Result<Spanned<Word>, Spanned<ParseError>>;

pub struct Parser<'a, TTokens>
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
        match self.next()? {
            Err(lex_error) => Some(self.make_error(Lex(lex_error.value.clone()))),
            Ok(token) => match &token.value {
                Token::Symbol => Some(self.parse_identifier()),
                Token::Number => Some(self.parse_integer()),
                Token::OpenBracket => Some(self.parse_array()),
                Token::OpenParenthesis => todo!(),
                Token::Plus => Some(self.parse_primitive_function(PrimitiveFunction::Add)),
                Token::Minus => Some(self.parse_primitive_function(PrimitiveFunction::Subtract)),
                Token::Star => Some(self.parse_primitive_function(PrimitiveFunction::Multiply)),
                Token::Slash => Some(self.parse_primitive_function(PrimitiveFunction::Divide)),
                Token::Ampersand => Some(self.parse_primitive_function(PrimitiveFunction::And)),
                Token::Pipe => Some(self.parse_primitive_function(PrimitiveFunction::Or)),
                Token::Caret => Some(self.parse_primitive_function(PrimitiveFunction::Xor)),
                Token::Bang => Some(self.parse_primitive_function(PrimitiveFunction::Not)),
                Token::Underscore => Some(self.parse_primitive_function(PrimitiveFunction::Negate)),
                Token::Equal => Some(self.parse_primitive_function(PrimitiveFunction::Equal)),
                Token::NotEqual => Some(self.parse_primitive_function(PrimitiveFunction::NotEqual)),
                Token::Greater => Some(self.parse_primitive_function(PrimitiveFunction::Greater)),
                Token::GreaterEqual => Some(self.parse_primitive_function(PrimitiveFunction::GreaterEqual)),
                Token::Less => Some(self.parse_primitive_function(PrimitiveFunction::Less)),
                Token::LessEqual => Some(self.parse_primitive_function(PrimitiveFunction::LessEqual)),
                _ => Some(self.make_error(ParseError::Unexpected(token.value.clone()))),
            },
        }
    }

    fn parse_integer(&mut self) -> ParseWordResult {
        let number = i64::from_str(self.lexeme(&self.span)).unwrap();
        self.make_word(Word::Integer(number))
    }

    fn parse_identifier(&mut self) -> ParseWordResult {
        match self.lexeme(&self.span) {
            "dup" => self.parse_primitive_function(PrimitiveFunction::Dup),
            "drop" => self.parse_primitive_function(PrimitiveFunction::Drop),
            "swap" => self.parse_primitive_function(PrimitiveFunction::Swap),
            "over" => self.parse_primitive_function(PrimitiveFunction::Over),
            name => self.make_word(Word::Symbol(name.to_string())),
        }
    }

    fn parse_primitive_function(&self, primitive: PrimitiveFunction) -> ParseWordResult {
        self.make_word(Word::Function(Box::new(Function::Primitive(primitive))))
    }

    fn parse_array(&mut self) -> ParseWordResult {
        self.parse_delimited(Word::Array, Token::CloseBracket)
    }

    fn parse_delimited(
        &mut self,
        word: impl Fn(Vec<Spanned<Word>>) -> Word,
        closing_token: Token,
    ) -> ParseWordResult
    {
        let start_span = self.span.clone();
        let mut elements = Vec::new();

        loop {
            match self.peek() {
                None => return self.make_error(ParseError::Expected(closing_token)),
                Some(Err(lex_error)) => {
                    let error = lex_error.value.clone();
                    self.next();
                    return self.make_error(Lex(error));
                }
                Some(Ok(token)) if token.value == closing_token => {
                    self.next();
                    self.span = start_span.merge(&self.span);
                    return self.make_word(word(elements));
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

    fn next(&mut self) -> Option<LexTokenResult> {
        self.tokens
            .next()
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
