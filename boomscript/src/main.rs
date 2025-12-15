// See: https://strlen.com/files/lang/false/false.txt

use std::collections::HashMap;

#[derive(Debug)]
enum FalseError {
    EmptyStack,
    ExpectedWord,
    ExpectedArray,
    ExpectedLambda,
}

#[derive(Debug)]
enum Value {
    Word(String),
    Array(Array),
    Lambda(Lambda),
}

#[derive(Debug)]
struct Array {
    shape: Shape,
    elements: Vec<i32>,
}

impl Array {
    fn new(shape: Shape, elements: Vec<i32>) -> Self {
        Self { shape, elements }
    }

    fn scalar(element: i32) -> Self {
        Self::new(Shape::EMPTY, vec![element])
    }
}

#[derive(Debug)]
struct Shape {
    dimensions: [usize; 3],
}

impl Shape {
    const EMPTY: Self = Self { dimensions: [0; 3] };
}


#[derive(Debug)]
struct Lambda {
    start_ip: usize,
    stop_ip: usize,
}

impl Value {
    fn as_word(&self) -> Result<&String, FalseError> {
        match self {
            Value::Word(word) => Ok(word),
            _ => Err(FalseError::ExpectedLambda)
        }
    }

    fn as_array(&self) -> Result<&Array, FalseError> {
        match self {
            Value::Array(array) => Ok(array),
            _ => Err(FalseError::ExpectedLambda)
        }
    }

    fn as_lambda(&self) -> Result<&Lambda, FalseError> {
        match self {
            Value::Lambda(lambda) => Ok(lambda),
            _ => Err(FalseError::ExpectedLambda)
        }
    }
}

#[derive(Debug)]
struct False<'a> {
    bindings: HashMap<String, Value>,
    stack: Vec<Value>,
    code: &'a [u8],
    ip: usize,
}

impl<'a> False<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            bindings: HashMap::new(),
            stack: Vec::new(),
            code: source.as_bytes(),
            ip: 0,
        }
    }

    fn eval(&mut self) -> Result<(), FalseError> {
        self.ip = 0;

        while self.ip < self.code.len() {
            self.eval_step()?;
        }

        Ok(())
    }

    fn eval_step(&mut self) -> Result<(), FalseError> {
        // macro_rules! unary_number {
        //     ($f: expr) => {{
        //         let a: i32 = self.pop()?.try_into()?;
        //         self.push(Value::Array($f(a)));
        //     }}
        // }
        //
        // macro_rules! binary_number {
        //     ($f: expr) => {{
        //         let b: i32 = self.pop()?.try_into()?;
        //         let a: i32 = self.pop()?.try_into()?;
        //         self.push(Value::Array($f(a, b)));
        //     }}
        // }

        match self.code[self.ip] {
            b' ' | b'\t' | b'\n' | b'\r' => {}
            b'#' => {
                while self.ip < self.code.len() && self.code[self.ip] != b'\n' {
                    self.ip += 1
                }
            }

            b'0'..=b'9' => {
                let mut n: i32 = 0;

                while self.ip < self.code.len() && matches!(self.code[self.ip],  b'0'..=b'9') {
                    n = n * 10 + (self.code[self.ip] - b'0') as i32;
                    self.ip += 1;
                }

                self.ip -= 1;
                self.push(Value::Array(Array::scalar(n)))
            }

            _ => {}
        }

        self.ip += 1;
        Ok(())
    }

    fn eval_lambda(&mut self, lambda: Lambda) -> Result<(), FalseError> {
        let before = self.ip;

        self.ip = lambda.start_ip;

        while self.ip <= lambda.stop_ip {
            self.eval_step()?;
        }

        self.ip = before;

        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Result<Value, FalseError> {
        self.stack.pop().ok_or(FalseError::EmptyStack)
    }

    fn peek(&mut self) -> Result<&Value, FalseError> {
        self.stack.last().ok_or(FalseError::EmptyStack)
    }

    fn pick(&mut self, n: usize) -> Result<&Value, FalseError> {
        self.stack.iter().nth_back(n).ok_or(FalseError::EmptyStack)
    }
}

fn main() {
    let mut false_evaluator = False::new("1 2 33");
    match false_evaluator.eval() {
        Ok(_) => {
            println!("Stack: {:?}", false_evaluator.stack);
            println!("Bindings: {:?}", false_evaluator.bindings)
        }
        Err(error) => eprintln!("{:?}", error)
    }
}
