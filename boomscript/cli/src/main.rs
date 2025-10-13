use compiler::parser::{parse, ParseResult};

fn main() {
    const SOURCE: &str = r#"
        -1 6 * "input.txt" lines
    "#;

    println!("{:?}", parse(SOURCE).collect::<Vec<ParseResult>>())
}
