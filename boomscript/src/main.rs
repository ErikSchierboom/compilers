use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    let code = "(1 2) %a !a";
    let code = "2 2 ^";

    // println!("{:?}", tokenize(code));
    println!("{:?}", interpret(code))
}
