pub struct SourceText {
    pub source_code: String
}

impl SourceText {
    pub fn from_str(source_code: String) -> Self {
        Self { source_code }
    }
}

#[derive(Clone, Debug)]
pub struct Span {
    pub begin: usize,
    pub end: usize
}

impl Span {
    pub fn empty() -> Self {
        Self { begin: 0, end: 0 }
    }

    pub fn advance(&mut self) {
        self.begin = self.end
    }
}
