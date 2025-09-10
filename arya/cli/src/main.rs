use compiler::location::LineEndings;
use compiler::parser::parse;

fn main() {
    let source = " 1 [1 8 3] [1 2 3 ; 4 5 6]";
    let line_endings = LineEndings::new(source);

    for result in parse(source) {
        match result {
            Ok(word) => {
                let location = line_endings.location(&word.span);
                eprintln!("Word at {}:\n{}", location, word.value)
            }
            Err(error) => {
                let location = line_endings.location(&error.span);
                eprintln!("Error at {}:\n{}", location, error.value)
            }
        }
    }


    // match interpret(&source) {
    //     Ok(mut values) => {
    //         while let Some(value) = values.pop() {
    //             println!("{:}", value)
    //         }
    //     }
    //     Err(error) => {
    //         let line_endings = LineEndings::new(source);
    //         let location = line_endings.location(&error.span);
    //         eprintln!("Error at {}: {}", location, error.value)
    //     }
    // }
}
