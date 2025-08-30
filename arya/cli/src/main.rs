use compiler::parser::parse;

fn main() {
    let source = "[1 8 3] [1 5 3] +";

    for parse_result in parse(source) {
        println!("{:?}", parse_result)
    }

    // match interpret(&source) {
    //     Ok(mut values) => {
    //         while let Some(value) = values.pop() {
    //             println!("{:}", value.value)
    //         }
    //     }
    //     Err(error) => {
    //         let line_endings = LineEndings::new(source);
    //         let location = line_endings.location(&error.span);
    //         eprintln!("Error at {}: {}", location, error.value)
    //     }
    // }
}
