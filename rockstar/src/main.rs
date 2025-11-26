use std::iter::Peekable;

#[derive(Debug)]
enum LexError {}

#[derive(Debug)]
enum Token {
    Number(f64),

    String(String),
    Empty,
    Silent,
    Silence,

    True,
    Right,
    Yes,
    Ok,

    False,
    Wrong,
    No,
    Lies,

    Null,
    Nothing,
    Nowhere,
    Nobody,
    Gone,

    Mysterious,

    Print,
    Scream,
    Shout,
    Whisper,
}

struct Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    chars: Peekable<TChars>,
}

impl<TChars> Lexer<TChars>
where
    TChars: Iterator<Item=char>,
{
    fn new(chars: TChars) -> Self {
        Self { chars: chars.peekable() }
    }

    fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(char) = self.chars.next() {
            match char {
                '0'..='9' => {
                    let mut number: String = String::new();
                    number.push(char);

                    while let Some(digit) = self.chars.next_if(char::is_ascii_digit) {
                        number.push(digit)
                    }

                    if let Some(dot) = self.chars.next_if_eq(&'.') {
                        number.push(dot);

                        while let Some(digit) = self.chars.next_if(char::is_ascii_digit) {
                            number.push(digit)
                        }
                    }

                    tokens.push(Token::Number(number.parse().unwrap()))
                },
                _ => {}
            }
        }

        Ok(tokens)
    }
}

fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source.chars());
    lexer.lex()
}

fn main() {
    const CODE: &str = "1 23.56";
    println!("{:?}", lex(CODE))
}
