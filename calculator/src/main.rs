use crate::lexer::tokenize;

mod lexer;
mod parser;

fn main() {
    const CODE: &str = "11 + -2 * (33 - 3)";

    println!("{:?}", tokenize(CODE))
}
