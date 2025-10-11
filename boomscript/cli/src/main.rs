use compiler::lexer::tokenize;

fn main() {
    const SOURCE: &str = r#"
        "input.txt" | read_file > $file
    "#;

    println!("{:?}", tokenize(SOURCE))
}
