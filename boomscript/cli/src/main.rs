use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        -1 [6 4 5] + ?
    "#;

    println!("{:?}", interpret(SOURCE))
}
