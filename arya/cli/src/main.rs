use compiler::interpreter::interpret;
use compiler::location::LineEndings;

fn main() {
    let source = "[1 8 3] (+) reduce";

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
