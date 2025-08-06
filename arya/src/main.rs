use crate::source::SourceText;
use crate::scanner::scan;

mod scanner;
mod source;

fn main() {
    let code: String = "# this is a comment\n+ [1 2 3] 2\n@w @\\n \"abc 123 %^@#\"".into();
    let source_text = SourceText::from_str(code);

    for scan_result in scan(&source_text) {
        match scan_result {
            Ok(token) => println!("Token: {:?} at {:?}", token.value, source_text.get_location(token.span)),
            Err(error) => eprintln!("Error: {:?} at {:?}", error.value, source_text.get_location(error.span))
        }
        
    }
}

// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests

