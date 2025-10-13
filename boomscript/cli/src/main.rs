use compiler::parser::{parse, ParseResult};

fn main() {
    const SOURCE: &str = r#"
        "input.txt" read-file lines -1 232
    "#;

    println!("{:?}", parse(SOURCE).collect::<Vec<ParseResult>>())
}
