use crate::parser::parse;
use crate::source::Source;

mod scanner;
mod source;
mod parser;

fn main() {
    let source = Source::from_text("# this is a comment\n+ [1 2 3] 2\n@w @\\n \"abc 123 %^@#\"".into());

    match parse(&source) {
        Ok(nodes) => println!("{:?}", nodes),
        Err(errors) => eprintln!("{:?}", errors),
    }
}

// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests

