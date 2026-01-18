use crate::interpreter::evaluate;

mod lexer;
mod parser;
mod recursive_descent_parser;
mod interpreter;

fn main() {
    const CODE: &str = "11 + 2 * (33 + -3)";

    println!("{:?}", evaluate(CODE))
}
