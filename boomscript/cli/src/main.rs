use compiler::parser::{parse, ParseResult};

fn main() {
    const SOURCE: &str = r#"
        -1 6 *
    "#;

    println!("{:?}", parse(SOURCE).collect::<Vec<ParseResult>>())
}
