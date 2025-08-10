use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::Path;

#[derive(Clone, Debug)]
pub struct Location {
    pub line: u16,
    pub column: u16,
    pub position: u32
}

impl Location {
    pub fn new(line: u16, column: u16, position: u32) -> Self {
        Self { line, column, position }
    }

    pub fn advance(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.position += 1
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug)]
pub struct Span {
    pub source: Source,
    pub begin: Location,
    pub end: Location
}

impl Span {
    pub(crate) fn new(source: Source, begin: Location, end: Location) -> Self {
        Self { source, begin, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.source {
            Source::Text(_) => write!(f, "{} - {}", self.begin, self.end),
            Source::File(_, path) => write!(f, "{}: {} - {}", path.to_str().unwrap(), self.begin, self.end)
        }
    }
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

#[derive(Clone, Debug)]
pub enum Source {
    Text(String),
    File(String, Box<Path>)
}

impl Source {
    pub fn source_code(&self) -> &String {
        match self {
            Source::Text(source) => source,
            Source::File(source, _) => source
        }
    }

    pub fn from_text(source: String) -> Self {
        Self::Text(source)
    }

    pub fn from_file(path: Box<Path>) -> Result<Self, Box<dyn Error>> {
        let source = fs::read_to_string(&path)?;
        Ok(Self::File(source, path))
    }
}
