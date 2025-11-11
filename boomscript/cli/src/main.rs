use compiler::parser::parse;

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
        let lines = read-lines("input.txt");
        let ints = map(lines, line -> parse-int(line));
        let windowed = windows(ints, 2);
        let filtered = filter(windows, kv -> key(kv) < value(kv));
        let part_1 = count(filtered);
    "#;

    match parse(SOURCE) {
        Ok(statements) => {
            print!("{:?} ", statements)
        }
        Err(error) => {
            eprintln!("Error: {:?}", error)
        }
    }
}
