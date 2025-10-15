use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        -1 [[6 4 5] [7 8 9]] + ?
    "#;

    println!("{:?}", interpret(SOURCE))
}
