use std::iter::Peekable;

#[derive(Debug)]
enum Token {
    Int(i64),
    Float(f64),
    Char(char),
    Str(String),
    Keyword(String),
    Identifier(String),

    True,
    False,
    Nil,
    Self_,
    Super,

    Plus,
    Minus,
    Star,
    Slash,

    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    Colon,
    Dot,
    Pipe,
    Caret,
    Semicolon,
    Hash,
    ColonEqual,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
}

#[derive(Debug)]
enum LexError {
    UnexpectedChar(char),
    UnknownIdentifier,
    UnexpectedEndOfFile,
}

struct Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    chars: Peekable<TChars>
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=char> {
    
    fn new(source_code: TChars) -> Self {
        Self { chars: source_code.peekable() }
    }

    fn lex(&mut self) -> Option<Result<Token, LexError>> {
        while self.chars.next_if(char::is_ascii_whitespace).is_some() {}

        let char = self.chars.next()?;
        let result = match char {
            '.' => Ok(Token::Dot),
            '|' => Ok(Token::Pipe),
            '^' => Ok(Token::Caret),
            ';' => Ok(Token::Semicolon),
            '#' => Ok(Token::Hash),
            '(' => Ok(Token::OpenParen),
            ')' => Ok(Token::CloseParen),
            '[' => Ok(Token::OpenBracket),
            ']' => Ok(Token::CloseBracket),
            '\'' => self.lex_string(),
            '0'..='9' => self.lex_number(char),
            '+' => {
                if self.chars.peek().map(char::is_ascii_digit).is_some() {
                    self.lex_number(char)
                } else {
                    Ok(Token::Plus)
                }
            },
            '-' => {
                if self.chars.peek().map(char::is_ascii_digit).is_some() {
                    self.lex_number(char)
                } else {
                    Ok(Token::Minus)
                }
            }
            '<' => {
                if self.chars.next_if_eq(&'=').is_some() {
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            },
            '>' => {
                if self.chars.next_if_eq(&'=').is_some() {
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            },
            ':' => {
                if self.chars.next_if_eq(&'=').is_some() {
                    Ok(Token::ColonEqual)
                } else {
                    Ok(Token::Colon)
                }
            },
            '@' => {
                if let Some(c) = self.chars.next() {
                    Ok(Token::Char(c))
                } else {
                    Err(LexError::UnexpectedEndOfFile)
                }
            },
            char => Err(LexError::UnexpectedChar(char))
        };
        
        Some(result)
    }

    fn lex_string(&mut self) -> Result<Token, LexError> {
        let mut str: String = String::new();

        while let Some(c) = self.chars.next() {
            if c == '\'' && self.chars.next_if_eq(&'\'').is_none() {
                break
            }

            str.push(c)
        }

        Ok(Token::Str(str))
    }

    fn lex_number(&mut self, digit: char) -> Result<Token, LexError> {
        let mut str: String = String::new();
        str.push(digit);

        while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
            str.push(c);
        }

        if let Some(dot) = self.chars.next_if_eq(&'.') {
            str.push(dot);

            while let Some(c) = self.chars.next_if(char::is_ascii_digit) {
                str.push(c);
            }

            Ok(Token::Float(str.parse().unwrap()))
        } else {
            Ok(Token::Int(str.parse().unwrap()))
        }
    }
}

impl<TChars> Iterator for Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}

fn tokenize(source: &str) -> impl Iterator<Item=Result<Token, LexError>> + '_ {
    let chars_with_index = source.chars();
    Lexer::new(chars_with_index)
}

fn main() {
    // let result: Result<Vec<Token>, LexError> = tokenize("'hallo' true false nil self super . | ^ ; ()[] ").collect();
    let result: Result<Vec<Token>, LexError> = tokenize("1 2.0 3.434 +13 -44 1.").collect();
    println!("{:?}", result);
}
