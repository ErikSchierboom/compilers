use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    // let code = "(1 2) $a @a %a";
    let code = "1 2 3 rot";

    // println!("{:?}", tokenize(code));
    println!("{:?}", interpret(code))
}
