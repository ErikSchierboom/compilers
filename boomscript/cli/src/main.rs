use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        [1 2 -1] abs
    "#;

    match interpret(SOURCE) {
        Ok(stack) => {
            for value in stack.iter().rev() {
                println!("{value}")
            }
        }
        Err(error) => {
            eprintln!("Error: {error}")
        }
    }
}
