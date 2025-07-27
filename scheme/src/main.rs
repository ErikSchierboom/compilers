use crate::parser::parse;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let parse_result = parse("(1 2 345 (foo bar))");
    println!("{:?}", parse_result);
}
