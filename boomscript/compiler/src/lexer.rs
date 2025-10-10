pub enum Token {
    Int(i64),
    Float(f64),
    String(String),
}

struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self { source, chars: source.chars().collect(), position: 0 }
    }

    fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(char) = self.current() {
            match char {
                c if c.is_ascii_whitespace() => continue,
                _ => panic!("Unexpected character")
            }
        }

        tokens
    }

    fn current(&mut self) -> Option<&char> {
        self.chars.get(self.position)
    }

    fn advance(&mut self) {
        self.position += 1
    }
}

pub fn tokenize(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source);
    lexer.lex()
}
