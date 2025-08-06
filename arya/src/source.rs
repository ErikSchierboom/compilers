use std::cell::OnceCell;

#[derive(Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub length: usize
}

struct Lines {
    line_offsets: Vec<usize>
}

impl Lines {
    pub fn new(source_code: &String) -> Self {
        Self { line_offsets: Self::get_line_offsets(source_code) }
    }
    
    fn get_line_offsets(source_code: &String) -> Vec<usize> {
        source_code
            .chars()
            .enumerate()
            .filter_map(|(i, c)| if i == 0 || c == '\n' { Some(i) } else { None })
            .collect()
    }
    
    pub fn get_location(&self, span: Span) -> Location {
        let line_pos = self.line_offsets
            .iter()
            .rposition(|line_offset| line_offset <= &span.begin)
            .unwrap_or(0);
        let line = line_pos + 1;
        let column = span.begin - self.line_offsets[line_pos] + 1;
        let length = span.length();
        Location { line, column, length }
    }
}

pub struct SourceText {
    pub source_code: String,
    lines: OnceCell<Lines>
}

impl SourceText {
    pub fn from_str(source_code: String) -> Self {
        Self { source_code, lines: OnceCell::new() }
    }

    pub fn get_location(&self, span: Span) -> Location {
        self.lines
            .get_or_init(|| Lines::new(&self.source_code))
            .get_location(span)
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
    
    pub fn length(&self) -> usize {
        self.end - self.begin
    }
}
