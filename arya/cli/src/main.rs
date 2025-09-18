use compiler::interpreter::interpret;
use compiler::lexer::tokenize;
use compiler::location::LineEndings;
use compiler::parser::parse;

fn main() {
    // let source = "1 [1 8 3] + [[1 2 3] [4 5 6] [8 dup dup]] (-)";
    let source = "[1 2 3] reverse";
    // let error_source = "? 1";

    // test_tokenize(source);
    // test_tokenize(error_source);
    // test_parse(source);
    // test_parse(error_source);
    test_interpret(source);
    // test_interpret(error_source);
}

fn test_tokenize(source: &str) {
    let line_endings = LineEndings::new(source);
    for result in tokenize(source) {
        match result {
            Ok(spanned_token) => {
                let location = line_endings.location(&spanned_token.span);
                eprintln!("Token at {}: {}", location, spanned_token.value)
            }
            Err(spanned_error) => {
                let location = line_endings.location(&spanned_error.span);
                eprintln!("Lex error at {}: {}", location, spanned_error.value)
            }
        }
    }
}

fn test_parse(source: &str) {
    let line_endings = LineEndings::new(source);
    for result in parse(source) {
        match result {
            Ok(spanned_word) => {
                let location = line_endings.location(&spanned_word.span);
                eprintln!("Word at {}:\n{}", location, spanned_word.value)
            }
            Err(spanned_error) => {
                let location = line_endings.location(&spanned_error.span);
                eprintln!("Parse error at {}:\n{}", location, spanned_error.value)
            }
        }
    }
}

fn test_interpret(source: &str) {
    match interpret(&source) {
        Ok(mut values) => {
            while let Some(value) = values.pop() {
                println!("{}", value)
            }
        }
        Err(error) => {
            let line_endings = LineEndings::new(source);
            let location = line_endings.location(&error.span);
            eprintln!("Error at {}: {}", location, error.value)
        }
    }
}
