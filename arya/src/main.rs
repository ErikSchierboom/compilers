use crate::lex::tokenize;

mod lex;
mod source;
mod parse;
mod interpret;

fn main() {
    let source = "# comment\n[1 2 3] 5 + 2 -";

    for result in tokenize(&source) {
        match result {
            Ok(token) => println!("{:?}", token),
            Err(error) => eprintln!("{:?}", error),
        }
    }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
