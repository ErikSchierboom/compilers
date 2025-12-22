use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;

fn main() {
    // let code = "1 %a [2 3 @a + +] %b !b";
    let code = "(2 3) !";
    // let code = "2 3 +";
    // let code = "1 2 over * dup";
    println!("{:?}", interpret(code))
}
