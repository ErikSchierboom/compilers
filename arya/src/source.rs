use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Location {
    pub line: u16,
    pub column: u16,
    pub position: u32
}

impl Location {
    pub fn new() -> Self {
        Self { line: 1, column: 1, position: 0 }
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

#[derive(Clone, Debug)]
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
            Source::Text(_) => write!(f, "{} - {}", self.begin, self.end)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }

    pub fn map_value<U>(self, func: impl Fn(T) -> U) -> Spanned<U> {
        let value = func(self.value);
        Spanned::<U> { value, span: self.span.clone() }
    }
}

#[derive(Clone, Debug)]
pub enum Source {
    Text(String)
}

impl Source {
    pub fn source_code(&self) -> &String {
        match self {
            Source::Text(source) => source
        }
    }

    pub fn from_text(source: String) -> Self {
        Self::Text(source)
    }
}
