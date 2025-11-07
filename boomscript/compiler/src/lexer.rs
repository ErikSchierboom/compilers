#[derive(Debug)]
pub enum LexError {
    UnexpectedEndOfFile,
    UnexpectedCharacter(char),
    ExpectedCharacter(char),
    InvalidEscape(char),
}

#[derive(Debug)]
pub enum Token {
    // Literals
    Number(i64),
    Char(char),
    String(String),
    Identifier(String),

    // Delimiters
    OpenParenthesis,
    CloseParenthesis,

    // Symbols
    Equal,
    Comma,
    Semicolon,
    LessThan,
    Minus,
    MinusGreaterThan,
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, LexError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = source_code.chars().peekable();

    while let Some(char) = chars.next() {
        if char.is_whitespace() {
            continue;
        }

        let token = match char {
            '(' => Token::OpenParenthesis,
            ')' => Token::CloseParenthesis,
            '=' => Token::Equal,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '-' => {
                if chars.next_if_eq(&'>').is_some() {
                    Token::MinusGreaterThan
                } else {
                    Token::Minus
                }
            }
            '<' => Token::LessThan,
            '\'' => {
                let c = chars.next().ok_or(LexError::UnexpectedEndOfFile)?;
                chars.next_if_eq(&'\'').ok_or(LexError::ExpectedCharacter('\''))?;
                Token::Char(c)
            }
            '"' => {
                let mut string = String::new();
                while let Some(c) = chars.next_if(|&c| c != '"') {
                    string.push(c);
                }
                chars.next_if_eq(&'"').ok_or(LexError::ExpectedCharacter('"'))?;
                Token::String(string)
            }
            c if c.is_ascii_digit() => {
                let mut number = c.to_digit(10).unwrap();
                while let Some(digit) = chars.next_if(char::is_ascii_digit) {
                    number = number * 10 + digit.to_digit(10).unwrap();
                }
                Token::Number(number as i64)
            }
            c if c.is_ascii_alphabetic() => {
                let mut identifier = char.to_string();
                while let Some(c) = chars.next_if(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '?')) {
                    identifier.push(c);
                }
                Token::Identifier(identifier)
            }
            _ => return Err(LexError::UnexpectedCharacter(char))
        };

        tokens.push(token)
    }

    Ok(tokens)
}
