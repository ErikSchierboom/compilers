// use crate::scanner::{scan, SyntaxError, Token};
//
// #[derive(Debug, Clone)]
// pub enum Expression {
//     Integer(i64),
//     Symbol(String),
//     List(Vec<Expression>),
// }
//
// struct Parser {
//     tokens: Vec<Token>,
//     errors: Vec<SyntaxError>,
//     current: usize
// }
//
// impl Parser {
//     fn new(tokens: Vec<Token>, scan_errors: Vec<SyntaxError>) -> Self {
//         Self { tokens, errors: scan_errors, current: 0 }
//     }
//
//     pub fn parse(&mut self) -> (Vec<Expression>, Vec<SyntaxError>) {
//         let mut nodes: Vec<Expression> = Vec::new();
//
//         while let Some(node_result) = self.parse_node() {
//             match node_result {
//                 Ok(node) => nodes.push(node),
//                 Err(error) => self.errors.push(error)
//             }
//         }
//
//         (nodes, self.errors.clone())
//     }
//
//     fn parse_node(&mut self) -> Option<Result<Expression, SyntaxError>> {
//         let token = self.advance()?;
//         match token {
//             Token::OpenParen => Some(self.list()),
//             Token::CloseParen => Some(Err(SyntaxError::UnexpectedCharacter(')'))),
//             Token::Identifier(identifier) => Some(Ok(Expression::Symbol(identifier.to_string()))),
//             Token::Integer(lexeme) => Some(Ok(Expression::Integer(lexeme.parse().unwrap())))
//         }
//     }
//
//     fn list(&mut self) -> Result<Expression, SyntaxError> {
//         let mut elements: Vec<Expression> = Vec::new();
//
//         loop {
//             if self.advance_if(|token| matches!(token, Token::CloseParen)).is_some() {
//                 return Ok(Expression::List(elements))
//             }
//
//             match self.parse_node() {
//                 Some(Ok(node)) => elements.push(node),
//                 Some(Err(error)) => return Err(error),
//                 None => return Err(SyntaxError::ExpectedCharacter(')'))
//             }
//         }
//     }
//
//     fn advance(&mut self) -> Option<&Token> {
//         self.advance_if(|_| true)
//     }
//
//     fn advance_if(&mut self, func: impl FnOnce(&Token) -> bool) -> Option<&Token> {
//         self.tokens.get(self.current)
//             .filter(|&token| func(token))
//             .inspect(|_| self.current += 1)
//     }
// }
//
// pub fn parse(source_code: &str) -> (Vec<Expression>, Vec<SyntaxError>) {
//     let (tokens, scan_errors) = scan(source_code);
//
//     let mut parser = Parser::new(tokens, scan_errors);
//     parser.parse()
// }