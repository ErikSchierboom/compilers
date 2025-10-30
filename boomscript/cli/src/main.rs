use compiler::lexer::tokenize;

fn main() {
    // const SOURCE: &str = r#"
    //     "input.txt"
    //         |> lines()
    //         |> map(&to-int)
    //         |> windows(2)
    //         |> filter(&(%0 < %1))
    //         |> count()
    // "#;

    // const SOURCE: &str = r#"
    //    # this is a comment
    //     count(filter(lt(&(elem(%,0), elem(%,1))), windows(map(lines("input.txt"), &(to-int(%))), 2))
    // "#;

    const SOURCE: &str = r#"
       # this is a comment
        lines("input.txt")
    "#;

    for lex_result in tokenize(SOURCE) {
        match lex_result {
            Ok(token) => {
                print!("{token} ")
            }
            Err(error) => {
                eprintln!("Error: {error}")
            }
        }
    }
}
