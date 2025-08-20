use crate::interpret::interpret;
use crate::location::LineEndings;

mod lex;
mod parse;
mod interpret;
mod location;

fn main() {
    let source = "4 [[1 2 3] [11 12 13]] +";

    match interpret(&source) {
        Ok(values) => {
            for value in values {
                println!("{:}", value.value)
            }
        },
        Err(error) => {
            let line_endings = LineEndings::new(source);
            let location = line_endings.location(&error.span);
            eprintln!("Error at {}: {}", location, error.value)
        },
    }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
