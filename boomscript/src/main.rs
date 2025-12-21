use crate::parser::parse;

mod lexer;
mod parser;

fn main() {
    let code = "12 %a @a [3 4 *] !";
    println!("{:?}", parse(code))
}
