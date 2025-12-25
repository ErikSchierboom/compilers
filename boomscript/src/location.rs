#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub const EMPTY: Self = Self { start: 0, end: 0 };

    pub fn merge(&self, other: &Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }

    pub fn followed_by(&self, other: &Self) -> bool {
        self.end == other.start
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::EMPTY
    }
}

// TODO: add functionality to map Span to line/column
// TODO: add diagnostic struct
// TODO: impl Display for diagnostic
