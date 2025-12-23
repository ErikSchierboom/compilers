use crate::lexer::tokenize;

mod lexer;
mod parser;
mod interpreter;

fn main() {
    // let code = "1 %a [2 3 @a + +] %b !b";
    // let code = "3 2 swap drop";
    let code = "12 3 +";
    // let code = "2 %a @a @a";
    let result = tokenize(code);
    println!("{:?}", result)
    // println!("{:?}", interpret(code))
}
