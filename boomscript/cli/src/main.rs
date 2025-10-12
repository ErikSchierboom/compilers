use compiler::lexer::tokenize;

fn main() {
    const SOURCE: &str = r#"
        lines(read_file("input.txt"))
    "#;

    println!("{:?}", tokenize(SOURCE))
}
