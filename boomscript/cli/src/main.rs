use compiler::lexer::tokenize;
// use compiler::parser::parse;

fn main() {
    const SOURCE: &str = r#"
        lines: "input.txt" read-lines
        ints: lines parse-int map  
        wins: ints 2 windows 
        filt: wins < filter
        filt count
    "#;

    match tokenize(SOURCE) {
        Ok(statements) => {
            print!("{:?} ", statements)
        }
        Err(error) => {
            eprintln!("Error: {:?}", error)
        }
    }
}
