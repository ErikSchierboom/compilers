#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(char)
}

#[derive(Debug)]
pub enum Token {
    Number(i64),
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, LexicalError> {
    let mut tokens = Vec::new();
    let mut chars = code.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            ' ' | '\t' | '\r' | '\n' => continue,
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '0'..='9' => {
                let mut number = c.to_digit(10).unwrap() as i64;

                while let Some(c) = chars.next_if(char::is_ascii_digit) {
                    number = number * 10 + c.to_digit(10).unwrap() as i64;
                }

                tokens.push(Token::Number(number))
            }
            _ => return Err(LexicalError::UnexpectedCharacter(c))
        }
    }

    Ok(tokens)
}
