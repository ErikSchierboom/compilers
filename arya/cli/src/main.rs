use compiler::interpreter::interpret;
use compiler::location::LineEndings;

fn main() {
    let source = "A: dup dup\n[1 8 3] [1 5 3] A";

    match interpret(&source) {
        Ok(mut values) => {
            while let Some(value) = values.pop() {
                println!("{:}", value.value)
            }
        }
        Err(error) => {
            let line_endings = LineEndings::new(source);
            let location = line_endings.location(&error.span);
            eprintln!("Error at {}: {}", location, error.value)
        }
    }
}
