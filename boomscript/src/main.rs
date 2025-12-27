use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    // let code = "(1 2) $a @a %a";
    let code = "#a 3 +";

    // println!("{:?}", tokenize(code));
    println!("{:?}", interpret(code))
}
