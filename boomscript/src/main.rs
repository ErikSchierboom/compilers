use std::fs;
use crate::lexer::tokenize;

mod lexer;
mod location;
mod parser;

fn main() {

    let code = fs::read_to_string("/Users/erik/Code/compilers/boomscript/examples/functions.bs").unwrap();
    println!("{:?}", tokenize(code.as_str()))
}
