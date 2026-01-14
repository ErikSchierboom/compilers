use std::fs;
use crate::lexer::tokenize;
use crate::parser::parse;

mod lexer;
mod location;
mod parser;

fn main() {

    let code = fs::read_to_string("/Users/erik/Code/compilers/boomscript/examples/expressions.bs").unwrap();
    println!("{:?}", parse(code.as_str()))
}
