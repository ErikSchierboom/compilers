// use crate::interpreter::interpret;
use crate::scanner::scan;

mod scanner;
// mod parser;
// mod interpreter;

fn main() {
    let result = scan("; this is a comment\n(+ 1 2 (* 3 6) #t #f #\\c car+qwe \"right on cue\")");
    println!("{:?}", result);
}
