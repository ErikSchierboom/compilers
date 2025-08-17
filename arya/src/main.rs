use crate::interpret::interpret;

mod lex;
mod source;
mod parse;
mod interpret;

fn main() {
    let source = "# comment\n+ [ [1 2 3] ] 5 ";

    match interpret(&source) {
        Ok(values) => println!("{:?}", values),
        Err(error) => eprintln!("{:?}", error),
    }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
