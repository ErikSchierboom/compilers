use crate::scanner::scan;

mod scanner;

fn main() {
    const code: &str = "
        # this is a comment
        + [1 2 3] 2
         @w @\\n \"abc 123 %^@#\"
     ";

    for token in scan(code) {
        println!("{:?}", token);
    }
}

// TODO: add clippy
// TODO: add unit tests
// TODO: add perf tests

