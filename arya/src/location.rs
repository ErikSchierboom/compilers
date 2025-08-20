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
