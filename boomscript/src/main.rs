use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    // let code = "(1 2) $a @a %a";
    let code = "[1 2 3] 2 max";

    // TODO: support quoting operators
    let code = "(2 3) (4 * +) ++";

    // println!("{:?}", tokenize(code));
    println!("{:?}", interpret(code))
}
