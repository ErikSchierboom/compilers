use crate::interpreter::interpret;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let result = interpret("(+ 1 2 (* 3 6))");
    println!("{:?}", result);
}
