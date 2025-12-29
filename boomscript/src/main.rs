use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;
mod lowering;

fn main() {
    let code = "(1 2) $a %a";
    // let code = "[1 2 3] 2 max";

    // TODO: support quoting operators
    // let code = "[2 3] [4 5] ++";
    // let code = "2 3 swap";

    // println!("{:?}", tokenize(code));
    println!("{:?}", interpret(code))
}
