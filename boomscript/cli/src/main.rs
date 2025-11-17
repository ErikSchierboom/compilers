use compiler::lexer::tokenize;
// use compiler::parser::parse;

fn main() {
    const SOURCE: &str = r#"
        let lines = "input.txt" read-lines
        let ints = lines parse-int map  
        let wins = ints 2 windows 
        let filt = wins < filter
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
