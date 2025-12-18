#[derive(Debug)]
enum Token {
    // Literals
    Int(i64),
    Float(f64),
    Identifier(String),

    // Operators
    Plus,
    Equal,

    // Keywords
    Let,

    // Whitespace
    Newline,
}

fn tokenize(code: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current = 0;
    let chars: Vec<char> = code.chars().collect();

    while current < chars.len() {
        match chars[current] {
            ' ' | '\r' | '\t' => current += 1,
            '\n' => {
                tokens.push(Token::Newline);
                current += 1
            }
            '+' => {
                tokens.push(Token::Plus);
                current += 1
            }
            '=' => {
                tokens.push(Token::Equal);
                current += 1
            }
            'a'..'z' | 'A'..'Z' => {
                let mut identifier = String::new();

                while current < chars.len() && chars[current].is_ascii_alphanumeric() {
                    identifier.push(chars[current]);
                    current += 1
                }

                match &identifier[..] {
                    "let" => tokens.push(Token::Let),
                    _ => tokens.push(Token::Identifier(identifier))
                }
            }
            '0'..'9' => {
                let mut number = String::new();

                while current < chars.len() && chars[current].is_ascii_alphanumeric() {
                    number.push(chars[current]);
                    current += 1
                }

                if current < chars.len() && chars[current] == '.' {
                    current += 1;

                    while current < chars.len() && chars[current].is_ascii_alphanumeric() {
                        number.push(chars[current]);
                        current += 1
                    }

                    tokens.push(Token::Float(number.parse().unwrap()))
                } else {
                    tokens.push(Token::Int(number.parse().unwrap()))
                }
            }
            _ => panic!("unknown token")
        }
    }

    tokens
}

fn main() {
    let code = "let x = 1\n\
                     let x1 = x + 2";

    dbg!(tokenize(code));
}
