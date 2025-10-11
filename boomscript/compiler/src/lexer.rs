#[derive(Debug)]
pub enum Token {
    Pipe,
    Int(i64),
    String(String),
    Function(String),
    Binding(String),
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter(char)
}

struct Lexer<'a> {
    source: &'a str,
    chars: Vec<char>,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().collect(),
            tokens: Vec::new(),
            errors: Vec::new(),
            position: 0,
        }
    }

    fn lex(mut self) -> (Vec<Token>, Vec<LexError>) {
        while let Some(char) = self.current() {
            match char {
                '|' => {
                    self.emit_token(Token::Pipe);
                    self.advance()
                }
                '"' => self.string(),
                '$' => self.binding(),
                c if c.is_ascii_digit() => self.integer(),
                c if c.is_ascii_alphabetic() => self.identifier(),
                c if c.is_ascii_whitespace() => self.whitespace(),
                c => {
                    self.emit_error(LexError::UnexpectedCharacter(c.clone()));
                    self.advance();
                }
            }
        }

        (self.tokens, self.errors)
    }

    fn binding(&mut self) {
        let start = self.position;
        self.advance();
        self.advance_while(|&c| c.is_ascii_alphanumeric() || c == '_');

        let str = self.source[start..self.position].to_string();
        self.emit_token(Token::Binding(str))
    }

    fn identifier(&mut self) {
        let start = self.position;
        self.advance_while(|&c| c.is_ascii_alphanumeric() || c == '_');

        let str = self.source[start..self.position].to_string();
        self.emit_token(Token::Function(str))
    }

    fn string(&mut self) {
        let start = self.position;
        self.advance();
        self.advance_while(|&c| c != '"');
        self.advance();

        let str = self.source[start..self.position].to_string();
        self.emit_token(Token::String(str))
    }

    fn integer(&mut self) {
        let start = self.position;
        self.advance();
        self.advance_while(char::is_ascii_digit);

        let int: i64 = self.source[start..self.position].parse().unwrap();
        self.emit_token(Token::Int(int))
    }

    fn emit_token(&mut self, token: Token) {
        self.tokens.push(token)
    }

    fn emit_error(&mut self, error: LexError) {
        self.errors.push(error)
    }

    fn whitespace(&mut self) {
        self.advance_while(char::is_ascii_whitespace);
    }

    fn current(&self) -> Option<&char> {
        self.chars.get(self.position)
    }

    fn advance(&mut self) {
        self.position += 1
    }

    fn advance_if(&mut self, condition: impl FnOnce(&char) -> bool) {
        if self.current().map(condition).unwrap_or_default() {
            self.advance()
        }
    }

    fn advance_while(&mut self, condition: impl Fn(&char) -> bool) {
        while self.current().map(&condition).unwrap_or_default() {
            self.advance()
        }
    }
}

pub fn tokenize(source: &str) -> (Vec<Token>, Vec<LexError>) {
    let lexer = Lexer::new(source);
    lexer.lex()
}
