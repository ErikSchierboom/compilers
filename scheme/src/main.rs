use crate::parser::parse;
// use crate::interpreter::interpret;
use crate::scanner::scan;

mod scanner;
mod parser;
// mod interpreter;

fn main() {
    let result = parse("; this is a comment\n(+ 12 -23 (* 3. 6.234) #t #f #\\c car+qwe \"right on cue\")");
    println!("{:?}", result);
}
