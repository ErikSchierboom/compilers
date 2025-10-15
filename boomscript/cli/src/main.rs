use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        [[1]]
    "#;

    println!("{:?}", interpret(SOURCE))
}
