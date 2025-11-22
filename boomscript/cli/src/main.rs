use compiler::parser::parse;

fn main() {
    const SOURCE: &str = r#"
        "input.txt" read-file lines 'int map 'numbers set [ 2 > ] filter @a
    "#;

    print!("{:?}", parse(SOURCE))
}
