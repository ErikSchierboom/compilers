use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub position: u32,
    pub length: u16,
}

impl Span {
    pub const EMPTY: Self = Self {
        position: 0,
        length: 0,
    };

    pub fn new(position: u32, length: u16) -> Self {
        Self { position, length }
    }

    pub fn merge(&self, rhs: &Self) -> Self {
        let (lhs, rhs) = if self.position <= rhs.position { (self, rhs) } else { (rhs, self) };

        Self::new(
            lhs.position,
            (rhs.position - lhs.position) as u16 + rhs.length,
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Location {
    pub line: u16,
    pub column: u16,
}

impl Location {
    pub fn new(line: u16, column: u16) -> Self {
        Self { line, column }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.line, self.column)
    }
}

pub struct LineEndings {
    line_starts: Vec<u32>,
}

impl LineEndings {
    pub fn new(source: &str) -> Self {
        let line_starts = std::iter::once(0)
            .chain(
                source
                    .match_indices('\n')
                    .map(|(position, _)| (position + 1) as u32),
            )
            .collect();
        Self { line_starts }
    }

    pub fn location(&self, span: &Span) -> Location {
        let line = self
            .line_starts
            .binary_search(&span.position)
            .unwrap_or_else(|next_line| next_line - 1) as u32
            + 1;
        let column = span.position
            - self
            .line_starts
            .get(line as usize - 1)
            .copied()
            .unwrap_or_default()
            + 1;
        Location::new(line as u16, column as u16)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge_adjacent_spans() {
        let lhs = Span::new(0, 1);
        let rhs = Span::new(1, 1);
        let result = lhs.merge(&rhs);
        assert_eq!(result.position, 0);
        assert_eq!(result.length, 2)
    }

    #[test]
    fn test_merge_non_adjacent_spans() {
        let lhs = Span::new(4, 3);
        let rhs = Span::new(9, 1);
        let result = lhs.merge(&rhs);
        assert_eq!(result.position, 4);
        assert_eq!(result.length, 6)
    }

    #[test]
    fn test_merge_is_order_independent() {
        let lhs = Span::new(2, 3);
        let rhs = Span::new(0, 2);

        let result = lhs.merge(&rhs);
        let flipped_result = rhs.merge(&lhs);

        assert_eq!(result, flipped_result)
    }

    #[test]
    fn test_location_from_span() {
        const SOURCE: &str = "1 2 3\n4 5 6";
        let line_endings = LineEndings::new(SOURCE);

        let first_character = Span::new(0, 1);
        assert_eq!(line_endings.location(&first_character), Location::new(1, 1));

        let middle_character = Span::new(9, 1);
        assert_eq!(
            line_endings.location(&middle_character),
            Location::new(2, 4)
        );

        let last_character = Span::new(10, 1);
        assert_eq!(line_endings.location(&last_character), Location::new(2, 5))
    }
}
