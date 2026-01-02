#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const EMPTY: Self = Self { start: 0, end: 0 };

    pub fn next_position(&self) -> Self {
        Self { start: self.start + 1, end: self.end + 1 }
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }

    pub fn is_contiguous_with(&self, other: &Self) -> bool {
        self.end == other.start
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::EMPTY
    }
}

#[derive(Debug)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }
}

#[derive(Debug)]
pub struct Position {
    pub line: u16,
    pub column: u16,
}

pub struct LineEndings {
    line_starts: Vec<usize>,
}

impl LineEndings {
    pub fn new(source: &str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(position, _)| position + 1))
            .collect();
        Self { line_starts }
    }

    pub fn location(&self, span: &Span) -> Position {
        let line = self.line_starts
            .binary_search(&span.start)
            .unwrap_or_else(|next_line| next_line - 1) as u16
            + 1;

        let column = (span.start - self.line_starts[line as usize - 1] + 1) as u16;

        Position { line, column }
    }
}
