use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;

fn main() {
    // let code = "1 %a [2 3 @a + +] %b !b";
    let code = "3 2 swap drop";
    // let code = "(2 3 +) [2 4]";
    // let code = "2 %a 3 %b (2 +) %c @b !c";
    let result = interpret(code);
    println!("{:?}", interpret(code))
}
