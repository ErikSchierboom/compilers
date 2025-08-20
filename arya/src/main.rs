use crate::interpret::interpret;
use crate::location::LineEndings;

mod lex;
mod parse;
mod interpret;
mod location;

fn main() {
    let source = "[ [1 2 3] [4 5 6] []] 5";

    match interpret(&source) {
        Ok(values) => println!("{:?}", values),
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
