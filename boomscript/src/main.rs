// use crate::interpreter::interpret;
use crate::lexer::tokenize;

mod lexer;
mod parser;
mod interpreter;
mod location;
mod lowering;
mod builtin;

fn main() {
    // let code = "(1 2 +) $a %a";
    // let code = "[1 2 3] 2 max";

    // let code = "[2 3] 'dup map";
    // let code = "2 3 swap";
    // let code = "#a #d <";
    let code = "\"a b c\"";

    println!("{:?}", tokenize(code));
    // println!("{:?}", interpret(code))
}
