use crate::interpreter::interpret;

mod scanner;
mod parser;
mod interpreter;

fn main() {
    let result = interpret(
        "; this is a comment
                    (+ 12 -23 (* 3. 6.234) #t #f #\\c car+qwe\
                    \"right on cue\")");
    println!("{:?}", result);
}
