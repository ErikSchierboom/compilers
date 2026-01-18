use crate::interpreter::evaluate;

mod lexer;
mod parser;

#[cfg(feature = "parser_recursive_descent")]
mod recursive_descent_parser;

#[cfg(feature = "parser_pratt")]
mod pratt_parser;

mod interpreter;

fn main() {
    const CODE: &str = "(-1 + 4) * 3 + 2";

    println!("{:?}", evaluate(CODE))
}
