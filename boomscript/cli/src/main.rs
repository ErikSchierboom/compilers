use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        [1 2 3] 5 dup
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
