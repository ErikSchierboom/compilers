use std::str::Chars;

#[derive(Debug)]
pub enum Token {
    LeftParen,
    RightParen,
    Int(i64),
    String(String),
    Identifier(String),
}

#[derive(Debug)]
pub enum LexError {
    UnexpectedCharacter(char)
}

struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    char: Option<char>,
    position: usize,
    tokens: Vec<Token>,
    errors: Vec<LexError>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let char = chars.next();

        Self {
            source,
            chars,
            char,
            tokens: Vec::new(),
            errors: Vec::new(),
            position: 0,
        }
    }

    fn lex(mut self) -> (Vec<Token>, Vec<LexError>) {
        while let Some(char) = self.char {
            match char {
                '(' => {
                    self.emit_token(Token::LeftParen);
                    self.advance()
                }
                ')' => {
                    self.emit_token(Token::RightParen);
                    self.advance()
                }
                '"' => self.string(),
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

    fn identifier(&mut self) {
        let start_pos = self.position;
        self.advance_while(|&c| c.is_ascii_alphanumeric() || c == '_');

        let str = self.source[start_pos..self.position].to_string();
        self.emit_token(Token::Identifier(str))
    }

    fn string(&mut self) {
        let start_pos = self.position;
        self.advance();
        self.advance_while(|&c| c != '"');
        self.advance();

        let str = self.source[start_pos..self.position].to_string();
        self.emit_token(Token::String(str))
    }

    fn integer(&mut self) {
        let start_pos = self.position;
        self.advance();
        self.advance_while(char::is_ascii_digit);

        let int: i64 = self.source[start_pos..self.position].parse().unwrap();
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

    fn advance(&mut self) {
        self.char = self.chars.next();
        self.position += 1
    }

    fn advance_if(&mut self, condition: impl FnOnce(&char) -> bool) {
        if self.char_matches(condition) {
            self.advance()
        }
    }

    fn advance_while(&mut self, condition: impl Fn(&char) -> bool) {
        while self.char_matches(&condition) {
            self.advance()
        }
    }

    fn char_matches(&self, condition: impl FnOnce(&char) -> bool) -> bool {
        self.char.as_ref().map(condition).unwrap_or_default()
    }
}

pub fn tokenize(source: &str) -> (Vec<Token>, Vec<LexError>) {
    let lexer = Lexer::new(source);
    lexer.lex()
}
