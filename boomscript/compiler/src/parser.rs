use crate::lexer::{tokenize, LexError, Token};

#[derive(Debug)]
pub enum ParseError {
    Lex(LexError),
    UnexpectedToken(Token),
    ExpectedToken(Token),
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Token, Expression),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Token),
    Binary(Box<Expression>, Token, Box<Expression>),
}

pub fn parse(source_code: &str) -> Result<Vec<Statement>, ParseError> {
    let tokens = tokenize(source_code).map_err(ParseError::Lex)?;
    let mut tokens = tokens.iter().peekable();
    let mut statements: Vec<Statement> = Vec::new();

    // while let Some(token) = tokens.next() {
    //     statements.push(parse_statement()?);
    // }

    Ok(statements)
}
