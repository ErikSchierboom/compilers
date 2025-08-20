use crate::interpret::interpret;
use crate::lex::tokenize;
use crate::parse::parse;

mod lex;
mod parse;
mod interpret;
mod location;

fn main() {
    let source = "[ [1 2 3] [4 5 6]] 5";

    for token_result in parse(&source) {
        match token_result {
            Ok(token) => println!("{:?}", token),
            Err(error) => eprintln!("{:?}", error),
        }
    }

    // match interpret(&source) {
    //     Ok(values) => println!("{:?}", values),
    //     Err(error) => eprintln!("{:?}", error),
    // }
}

// TODO: add CI
// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests
