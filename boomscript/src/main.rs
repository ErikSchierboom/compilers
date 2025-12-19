use crate::inference::infer;
use crate::parser::parse;

mod lexer;
mod parser;
mod inference;

fn main() {
    let code = "let x = 1\n\
                     let x1 = x + 2 * 3";

    for expr in parse(code) {
        let type_ = infer(&expr);
        println!("{:?} = {:?}", expr, type_)
    }
}
