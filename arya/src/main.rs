use crate::source::SourceText;
use crate::scanner::scan;

mod scanner;
mod source;

fn main() {
    let code: String = "
        # this is a comment
        + [1 2 3] 2
         @w @\\n \"abc 123 %^@#\"
     ".into();
    let source_text = SourceText::from_str(code);

    for token in scan(&source_text) {
        println!("{:?}", token);
    }
}

// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests

