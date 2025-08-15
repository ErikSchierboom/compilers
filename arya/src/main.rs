use crate::parse::parse;

mod lex;
mod source;
mod parse;
mod interpret;

fn main() {
    let source = "# comment\n[1 2 3] 5 + 2 -";

    match parse(&source) {
        Ok(nodes) => println!("{:?}", nodes),
        Err(error) => eprintln!("{:?}", error),
    }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
