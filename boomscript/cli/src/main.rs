use compiler::parser::parse;

fn main() {
    const SOURCE: &str = r#"
        "input.txt" read-file lines 'int map 'numbers set [ 2 > ] filter
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
