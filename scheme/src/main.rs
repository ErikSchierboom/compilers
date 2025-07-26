use crate::scanner::scan;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let tokens = scan("(1 2 345 foo bar)");
    println!("{:?}", tokens);
}
