use compiler::parser::{parse, ParseWordResult};

fn main() {
    let source = " 1 [1 8 3] [1 2 3 ; 4 5 6]";

    println!("{:?}", parse(source).collect::<Vec<ParseWordResult>>())

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
