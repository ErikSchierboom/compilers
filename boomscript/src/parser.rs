// use crate::lexer::{tokenize, LexError, Token};
// use crate::location::{Span, Spanned};
// use crate::parser::ParseError::Lex;
// use std::iter::Peekable;
// 
// #[derive(Debug)]
// pub enum ParseError {
//     Lex(LexError),
//     UnexpectedToken(String),
//     UnexpectedEndOfFile,
// }
// 
// impl From<LexError> for ParseError {
//     fn from(value: LexError) -> Self {
//         Lex(value)
//     }
// }
// 
// #[derive(Clone, Debug)]
// pub enum Word {
//     // Literals
//     Int(i64),
//     Float(f64),
//     Char(char),
//     String(String),
//     Quote(String),
//     Name(String),
// 
//     // Composite
//     Block(Vec<Word>),
//     Array(Vec<Word>),
// }
// 
// struct Parser<'a, T: Iterator<Item=Spanned<Token>>> {
//     code: &'a str,
//     tokens: Peekable<T>,
// }
// 
// impl<'a, T: Iterator<Item=Spanned<Token>>> Parser<'a, T> {
//     fn new(code: &'a str, tokens: T) -> Self {
//         Self { code, tokens: tokens.peekable() }
//     }
// 
//     fn parse(mut self) -> Result<Vec<Spanned<Word>>, Vec<Spanned<ParseError>>> {
//         let mut words = Vec::new();
//         let mut errors = Vec::new();
// 
//         while let Some(result) = self.parse_word() {
//             match result {
//                 Ok(word) => words.push(word),
//                 Err(parse_error) => errors.extend(parse_error)
//             }
//         }
// 
//         if errors.is_empty() {
//             Ok(words)
//         } else {
//             Err(errors)
//         }
//     }
// 
//     fn parse_word(&mut self) -> Option<Result<Spanned<Word>, Vec<Spanned<ParseError>>>> {
//         let spanned = self.tokens.next()?;
//         let Spanned(token, location) = spanned;
// 
//         match token {
//             Token::Int => {
//                 let value = self.lexeme(&location).parse().unwrap();
//                 Some(Ok(Spanned(Word::Int(value), location)))
//             }
//             Token::Float => {
//                 let value = self.lexeme(&location).parse().unwrap();
//                 Some(Ok(Word::Float(value, location)))
//             }
//             Token::Char => {
//                 let value = match &self.lexeme(&location)[1..] {
//                     "\\n" => '\n',
//                     "\\r" => '\r',
//                     "\\t" => '\t',
//                     "\\'" => '\'',
//                     lexeme => lexeme.chars().next().unwrap()
//                 };
//                 Some(Ok(Word::Char(value, location)))
//             }
//             Token::String => {
//                 let value = String::from(&self.lexeme(&location)[1..location.end - location.start - 1])
//                     .replace("\\n", "\n")
//                     .replace("\\t", "\t")
//                     .replace("\\r", "\r")
//                     .replace("\\\"", "\"");
//                 Some(Ok(Word::String(value, location)))
//             }
//             Token::Quote => {
//                 let name = self.lexeme(&location)[1..].into();
//                 Some(Ok(Word::Quote(name, location)))
//             }
//             Token::Word => {
//                 let name = self.lexeme(&location).into();
//                 Some(Ok(Word::Name(name, location)))
//             }
//             Token::OpenBracket => {
//                 match self.parse_delimited(|token| matches!(token, Token::CloseBracket(_)), location) {
//                     Ok((words, location)) => Some(Ok(Word::Array(words, location))),
//                     Err(err) => Some(Err(err))
//                 }
//             }
//             Token::OpenParen => {
//                 match self.parse_delimited(|token| matches!(token, Token::CloseParen(_)), location) {
//                     Ok((words, location)) => Some(Ok(Word::Block(words, location))),
//                     Err(err) => Some(Err(err))
//                 }
//             }
//             Token::CloseBracket |
//             Token::CloseParen => Some(Err(vec![ParseError::UnexpectedToken(self.lexeme(&location).into(), location.clone())])),
//         }
//     }
// 
//     fn parse_delimited(&mut self, stop_parsing: impl Fn(&Token) -> bool, start: Span) -> Result<(Vec<Word>), Vec<ParseError>> {
//         let mut words = Vec::new();
//         let mut errors = Vec::new();
// 
//         loop {
//             if let Some(token) = self.tokens.next_if(|token| stop_parsing(token)) {
//                 let location = start.merge(&token.location());
//                 if errors.is_empty() {
//                     return Ok((words, location));
//                 } else {
//                     return Err(errors);
//                 }
//             }
// 
//             match self.parse_word() {
//                 None => {
//                     errors.push(ParseError::UnexpectedEndOfFile(Span { start: self.code.len(), end: self.code.len() + 1 }));
//                     return Err(errors);
//                 }
//                 Some(Ok(word)) => words.push(word),
//                 Some(Err(err)) => errors.extend(err)
//             }
//         }
//     }
// 
//     fn lexeme(&self, location: &Span) -> &'a str {
//         &self.code[location.start..location.end]
//     }
// }
// 
// pub fn parse(code: &str) -> Result<Vec<Spanned<Word>>, Vec<Spanned<ParseError>>> {
//     match tokenize(code) {
//         Ok(tokens) => {
//             let parser = Parser::new(code, tokens.into_iter());
//             parser.parse()
//         }
//         Err(errors) => {
//             Err(errors
//                 .into_iter()
//                 .map(|error| error.map(ParseError::from))
//                 .collect())
//         }
//     }
// }
