#[derive(Clone, Debug)]
pub struct Span {
    pub position: u32,
    pub length: u16
}

impl Span {
    pub fn new(position: u32, length: u16) -> Self {
        Self { position, length }
    }

    pub fn merge(&self, rhs: &Self) -> Self {
        assert!(rhs.position >= self.position);
        Self::new(self.position, (rhs.position - self.position) as u16 + rhs.length)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_merge() {
        let lhs = Span::new(4, 3);
        let rhs = Span::new(9, 1);
        let result = lhs.merge(&rhs);
        assert_eq!(result.position, 4);
        assert_eq!(result.length, 6);
    }

    #[test]
    #[should_panic]
    fn test_bad_merge() {
        let lhs = Span::new(2, 3);
        let rhs = Span::new(0, 2);
        lhs.merge(&rhs);
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}
