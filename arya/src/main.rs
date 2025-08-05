use crate::scanner::scan;

mod scanner;

fn main() {
    const code: &str = "+ [1 2 3] 2";

    for token in scan(code) {
        println!("{:?}", token);
    }
}
