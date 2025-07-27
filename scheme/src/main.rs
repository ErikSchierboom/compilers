use crate::interpreter::interpret;
use crate::parser::parse;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let result = interpret("(+ 1 2 3)");
    println!("{:?}", result);
}
