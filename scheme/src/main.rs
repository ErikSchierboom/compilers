use crate::interpreter::interpret;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let result = interpret(
        "; this is a comment
                    (+ 12 -23)");
    println!("{:?}", result);
}
