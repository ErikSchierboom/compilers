use crate::inference::infer;
use crate::parser::parse;

mod lexer;
mod parser;
mod inference;

fn main() {
    let code = "let x = 1\n\
                let x1 = x < 2 * 3";

    let expressions = parse(code);

    let types = infer(&expressions);
    for (expr, type_) in expressions.iter().zip(types) {
        println!("{:?} = {:?}", expr, type_)
    }
}
