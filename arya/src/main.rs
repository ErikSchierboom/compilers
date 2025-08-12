use crate::parser::parse;
use crate::source::Source;

mod scanner;
mod source;
mod parser;

fn main() {
    let source = Source::from_text("# comment\n+ [1 2 3] 5".into());

    match parse(&source) {
        Ok(nodes) => nodes.iter().for_each(|node|println!("{}", node)),
        Err(errors) => errors.iter().for_each(|error|eprintln!("{}", error)),
    }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
