use crate::scanner::scan;
use crate::source::Source;

mod scanner;
mod source;

fn main() {
    let source = Source::from_text("# this is a comment\n+ [1 2 3] 2\n@w @\\n \"abc 123 %^@#\"".into());

    for scan_result in scan(&source) {
        match scan_result {
            Ok(token) => println!("Token: {} at {}", token.value, token.span),
            Err(error) => eprintln!("Error: {} at {}", error.value, error.span)
        }
    }
}

// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests

