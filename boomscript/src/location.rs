use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Span {
    pub start: u16,
    pub end: u16,
}

impl Span {
    pub const EMPTY: Self = Self { start: 0, end: 0 };

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end)
        }
    }
}

#[derive(Debug)]
pub struct Position {
    pub line: u16,
    pub column: u16,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{})", self.line, self.column)
    }
}

pub struct LineEndings {
    line_starts: Vec<u16>,
}

impl LineEndings {
    pub fn new(source: &str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(position, _)| (position as u16) + 1))
            .collect();
        Self { line_starts }
    }

    pub fn position(&self, span: &Span) -> Position {
        let line = self.line_starts
            .binary_search(&span.start)
            .unwrap_or_else(|next_line| next_line - 1) as u16
            + 1;
        let column = span.start - self.line_starts[line as usize - 1] + 1;

        Position { line, column }
    }
}
