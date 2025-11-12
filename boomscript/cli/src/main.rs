use compiler::parser::parse;

fn main() {
    const SOURCE: &str = r#"
        "input.txt"
            | read-lines
            | map parse-int
            | windows 2
            | filter lt
            | count
    "#;

    match parse(SOURCE) {
        Ok(statements) => {
            print!("{:?} ", statements)
        }
        Err(error) => {
            eprintln!("Error: {:?}", error)
        }
    }
}
