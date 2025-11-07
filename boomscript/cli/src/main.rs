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
        lines = read-lines("input.txt");
        ints = map(lines, line -> parse-int(line));
        windowed = windows(ints, 2);
        filtered = filter(windows, kv -> key(kv) < value(kv));
        part_1 = count(filtered);
    "#;

    match tokenize(SOURCE) {
        Ok(tokens) => {
            print!("{:?} ", tokens)
        }
        Err(error) => {
            eprintln!("Error: {:?}", error)
        }
    }
}
