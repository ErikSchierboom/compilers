use compiler::lexer::{tokenize, LexTokenResult};

fn main() {
    const SOURCE: &str = r#"
        "input.txt" read-file lines -1
    "#;

    println!("{:?}", tokenize(SOURCE).collect::<Vec<LexTokenResult>>())
}
