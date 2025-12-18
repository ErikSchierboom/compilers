use crate::parser::parse;

mod lexer;
mod parser;

fn main() {
    let code = "let x = 1\n\
                     let x1 = x + 2 * 3";

    dbg!(parse(code));
}
