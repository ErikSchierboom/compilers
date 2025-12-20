use std::iter::Peekable;

#[derive(Clone, Debug)]
pub enum Token {
    // Literals
    Bool(bool),
    Int(i64),
    Float(f64),
    Variable(String),

    // Operators
    Plus,
    Equal,
    Star,
    Greater,
    Less,
    RightArrow,

    // Keywords
    Let,
    Fn,

    // Whitespace
    Newline,
}

struct Lexer<T: Iterator<Item=char>> {
    chars: Peekable<T>,
}

impl<T: Iterator<Item=char>> Lexer<T> {
    fn new(code: T) -> Self {
        Self { chars: code.peekable() }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(char) = self.chars.next() {
            match char {
                ' ' | '\r' | '\t' => continue,
                '\n' => tokens.push(Token::Newline),
                '+' => tokens.push(Token::Plus),
                '*' => tokens.push(Token::Star),
                '=' => tokens.push(Token::Equal),
                '>' => tokens.push(Token::Greater),
                '<' => tokens.push(Token::Less),
                '-' => {
                    if self.chars.next_if_eq(&'>').is_some() {
                        tokens.push(Token::RightArrow)
                    } else {
                        panic!("unknown token")
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut identifier = String::new();
                    identifier.push(char);

                    while let Some(char) = self.chars.next_if(char::is_ascii_alphanumeric) {
                        identifier.push(char);
                    }

                    match &identifier[..] {
                        "let" => tokens.push(Token::Let),
                        "true" => tokens.push(Token::Bool(true)),
                        "false" => tokens.push(Token::Bool(false)),
                        "fn" => tokens.push(Token::Fn),
                        _ => tokens.push(Token::Variable(identifier))
                    }
                }
                '0'..='9' => {
                    let mut number = String::new();
                    number.push(char);

                    while let Some(char) = self.chars.next_if(char::is_ascii_digit) {
                        number.push(char);
                    }

                    if self.chars.next_if_eq(&'.').is_some() {
                        while let Some(char) = self.chars.next_if(char::is_ascii_digit) {
                            number.push(char);
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
}

pub fn tokenize(code: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(code.chars());
    lexer.tokenize()
}
