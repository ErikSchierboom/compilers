use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    // let code = "1 %a [2 3 @a + +] %b !b";
    // let code = "3 2 swap drop";
    let code = "12 %a @a";
    // let code = "2 %a @a @a";
    let result = interpret(code);
    println!("{:?}", interpret(code))
}
