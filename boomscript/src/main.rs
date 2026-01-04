use crate::interpreter::interpret;

mod lexer;
mod parser;
mod interpreter;
mod location;
mod lowering;
mod builtin;
mod diagnostic;

fn main() {
    // let code = "(1 2 +) $a %a";
    // let code = "2 #a +";

    // let code = "[2 3] 'dup map";
    let code = "(3 $a @a .) %";
    // let code = "#a #d <";

    // println!("{:?}", tokenize(code));

    match interpret(code) {
        Ok(values) => println!("{:?}", values),
        Err(diagnostics) => {
            for diagnostic in diagnostics {
                println!("{}", diagnostic)
            }
        }
    }
}
