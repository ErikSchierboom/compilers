#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(char)
}

#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Number,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexicalError> {
    let mut tokens = Vec::new();
    let mut chars = code.chars().enumerate().peekable();

    while let Some((start, c)) = chars.next() {
        match c {
            ' ' | '\t' | '\r' | '\n' => continue,
            '(' => tokens.push(Token { kind: TokenKind::LParen, span: Span { start, end: start + 1 }}),
            ')' => tokens.push(Token { kind: TokenKind::RParen, span: Span { start, end: start + 1 }}),
            '+' => tokens.push(Token { kind: TokenKind::Plus,   span: Span { start, end: start + 1 }}),
            '-' => tokens.push(Token { kind: TokenKind::Minus,  span: Span { start, end: start + 1 }}),
            '*' => tokens.push(Token { kind: TokenKind::Star,   span: Span { start, end: start + 1 }}),
            '/' => tokens.push(Token { kind: TokenKind::Slash,  span: Span { start, end: start + 1 }}),
            '0'..='9' => {
                let mut end = start + 1;

                while let Some((next_start, _)) = chars.next_if(|(_, next_c)| next_c.is_ascii_digit()) {
                    end = next_start + 1;
                }

                tokens.push(Token { kind: TokenKind::Number, span: Span { start, end } })
            }
            _ => return Err(LexicalError::UnexpectedCharacter(c))
        }
    }

    Ok(tokens)
}
