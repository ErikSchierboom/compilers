use compiler::interpreter::interpret;

fn main() {
    const SOURCE: &str = r#"
        "input.txt" read-file lines 'int map 'numbers set [ 2 > ] filter @a
    "#;

    print!("{:?}", interpret(SOURCE))
}
