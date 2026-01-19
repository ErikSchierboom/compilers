use crate::compiler::Compiler;
use crate::interpreter::Interpreter;
use crate::optimizer::optimize;
use crate::parser::Parser;

mod scanner;
mod parser;
mod compiler;
mod interpreter;
mod optimizer;

fn main() {
    let mut parser = Parser::new("3 0 *");
    let (parsed, errors) = parser.parse();
    
    if errors.len() > 0 {
        eprintln!("Errors: {:?}", errors);
        return;
    }
    
    let optimized = optimize(parsed);

    let compiler = Compiler::new(optimized);
    let bytecode = compiler.compile();

    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(bytecode);
    println!("Result:");
    println!("{:?}", result);
}
