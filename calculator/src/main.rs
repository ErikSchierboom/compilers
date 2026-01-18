use crate::interpreter::evaluate;

mod lexer;
mod parser;

#[cfg(feature = "parser_recursive_descent")]
mod recursive_descent_parser;

#[cfg(feature = "parser_pratt")]
mod pratt_parser;

mod interpreter;

fn main() {
    // const CODE: &str = "(1 + 2) * 3";
    const CODE: &str = "(1 + 2) * 3";
    // const CODE: &str = "11 + 2 * (33 + -3)";

    println!("{:?}", evaluate(CODE))
}
