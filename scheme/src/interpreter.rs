use crate::parser::parse;
use crate::scanner::{ScanError, Token};

#[derive(Debug, Clone)]
pub enum ParseError {
    ScanError(ScanError),
    ExpectedToken(Token),
    UnexpectedToken(Token)
}

pub fn interpret<'a>(source_code: &'a str) {
    let (nodes, errors) = parse(source_code);
}