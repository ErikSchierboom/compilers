use crate::interpreter::interpret;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let result = interpret(
        "; this is a comment
                    (define x 10)
                    (+ x 12 -23)");
    println!("{:?}", result);
}
