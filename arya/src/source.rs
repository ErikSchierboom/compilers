use std::cell::OnceCell;

#[derive(Debug)]
pub struct Location<'a> {
    pub text: &'a str,
    pub line: usize,
    pub column: usize,
    pub length: usize
}

struct Lines<'a> {
    source_code: &'a String,
    line_offsets: Vec<usize>
}

impl<'a> Lines<'a> {
    pub fn new(source_code: &'a String) -> Self {
        Self {
            source_code,
            line_offsets: Self::get_line_offsets(source_code)
        }
    }

    fn get_line_offsets(source_code: &'a String) -> Vec<usize> {
        source_code
            .chars()
            .enumerate()
            .filter_map(|(i, c)| if i == 0 || c == '\n' { Some(i) } else { None })
            .collect()
    }

    pub fn get_location(&self, span: Span) -> Location<'a> {
        let line_pos = self.line_offsets
            .iter()
            .rposition(|line_offset| line_offset <= &span.begin)
            .unwrap_or(0);
        let line = line_pos + 1;
        let column = span.begin - self.line_offsets[line_pos] + 1;
        let length = span.length();
        let text = &self.source_code[span.begin..span.end];
        Location { text, line, column, length }
    }
}

pub struct SourceText<'a> {
    pub source_code: String,
    lines: OnceCell<Lines<'a>>
}

impl<'a> SourceText<'a> {
    pub fn from_str(source_code: String) -> Self {
        Self { source_code, lines: OnceCell::new() }
    }

    pub fn get_location(&'a self, span: Span) -> Location {
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
