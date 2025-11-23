use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Sub};

#[derive(Debug)]
enum FalseError {
    EmptyStack,
    ExpectedInteger,
    ExpectedLambda,
}

#[derive(Clone, Copy, Debug)]
enum Value {
    Integer(i32),
    Lambda(usize, usize)
}

impl TryFrom<Value> for i32 {
    type Error = FalseError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Integer(i) => Ok(i),
            _ => Err(FalseError::ExpectedInteger)
        }
    }
}

impl TryFrom<Value> for (usize, usize) {
    type Error = FalseError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Lambda(start, end) => Ok((start, end)),
            _ => Err(FalseError::ExpectedLambda)
        }
    }
}

impl Into<Value> for i32 {
    fn into(self) -> Value {
        Value::Integer(self)
    }
}

impl Into<Value> for (usize, usize) {
    fn into(self) -> Value {
        Value::Lambda(self.0, self.1)
    }
}

#[derive(Debug)]
struct False {
    variables: [Value; 26],
    stack: Vec<Value>,
    chars: Vec<char>,
    ip: usize
}

impl False {
    fn new(source: &str) -> Self {
        Self {
            variables: [Value::Integer(0); 26],
            stack: Vec::new(),
            chars: source.chars().collect(),
            ip: 0
        }
    }

    fn eval(&mut self) -> Result<(), FalseError> {
        self.ip = 0;

        while self.ip < self.chars.len() {
            self.eval_step()?;
            self.ip += 1;
        }

        Ok(())
    }

    fn eval_step(&mut self) -> Result<(), FalseError> {
        macro_rules! unary_number {
            ($f: expr) => {{
                let a: i32 = self.pop()?.try_into()?;
                self.push(Value::Integer($f(a)));
            }}
        }

        macro_rules! binary_number {
            ($f: expr) => {{
                let b: i32 = self.pop()?.try_into()?;
                let a: i32 = self.pop()?.try_into()?;
                self.push(Value::Integer($f(a, b)));
            }}
        }

        match self.chars[self.ip] {
            ' ' | '\t' | '\n' | '\r' => {},
            '{' => {
                while self.ip < self.chars.len() && self.chars[self.ip] != '}' {
                    self.ip += 1
                }
            }

            '0'..='9' => {
                let mut n: i32 = 0;

                while self.ip < self.chars.len() && matches!(self.chars[self.ip],  '0'..='9') {
                    n = n * 10 + (self.chars[self.ip] as u8 - b'0') as i32;
                    self.ip += 1;
                }

                self.ip -= 1;
                self.push(n.into())
            },
            '\'' => {
                self.ip += 1;
                self.push((self.chars[self.ip] as i32).into())
            },

            '_' => unary_number!(i32::neg),
            '~' => unary_number!(i32::not),

            '&' => binary_number!(i32::bitand),
            '|' => binary_number!(i32::bitor),
            '=' => binary_number!(|a, b| if a == b { -1 } else { 0 }),
            '<' => binary_number!(|a, b| if a < b { -1 } else { 0 }),
            '>' => binary_number!(|a, b| if a > b { -1 } else { 0 }),
            '+' => binary_number!(i32::add),
            '-' => binary_number!(i32::sub),
            '*' => binary_number!(i32::mul),
            '/' => binary_number!(i32::div),

            'a'..='z' => self.push(((self.chars[self.ip] as u8 - b'a') as i32).into()),
            'A'..='Z' => self.push(self.variables[(self.chars[self.ip] as u8 - b'A') as usize]),

            ':' => {
                let var_addr: i32 = self.pop()?.try_into()?;
                let value = self.pop()?;
                self.variables[var_addr as usize] = value
            },
            ';' => {
                let var_addr: i32 = self.pop()?.try_into()?;
                self.push(self.variables[var_addr as usize]);
            },
            '[' => {
                self.ip += 1;
                let start = self.ip;
                let mut depth = 0;

                while self.ip < self.chars.len() {
                    match self.chars[self.ip] {
                        '[' => depth += 1,
                        ']' => {
                            if depth == 0 {
                                break
                            }

                            depth -= 1;
                        },
                        _ => {}
                    }

                    self.ip += 1;
                }

                let end = self.ip - 1;
                self.push(Value::Lambda(start, end))
            },
            '!' => {
                let before = self.ip;
                let (start, end) = self.pop()?.try_into()?;

                self.ip = start;

                while self.ip < end {
                    self.eval_step()?;
                }

                self.ip = before;
            },
            '?' => {
                let before = self.ip;

                let (start, end): (usize, usize) = self.pop()?.try_into()?;
                let bool: i32 = self.pop()?.try_into()?;

                if bool != 0 {
                    self.ip = start;

                    while self.ip < end {
                        self.eval_step()?;
                    }

                    self.ip = before;
                }
            }

            '%' => {
                self.pop()?;
            },
            '$' => {
                let a = self.pop()?;
                self.push(a.clone());
                self.push(a);
            },
            '\\' => {
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(b);
                self.push(a);
            },
            '@' => {
                let c = self.pop()?;
                let b = self.pop()?;
                let a = self.pop()?;
                self.push(b);
                self.push(c);
                self.push(a);
            },
            'ø' => {
                let n: i32 = self.pop()?.try_into()?;
                let nth_val = self.pick(n as usize)?.clone();
                self.push(nth_val)
            }

            '.' => {
                let i: i32 = (*self.peek().unwrap()).try_into()?;
                print!("{}", i)
            }
            ',' => {
                let i: i32 = (*self.peek().unwrap()).try_into()?;
                print!("{}", i as u8 as char)
            },
            '"' => {
                self.ip += 1;
                while self.ip < self.chars.len() && self.chars[self.ip] != '"' {
                    print!("{}", self.chars[self.ip] as u8 as char);
                    self.ip += 1;
                }
            }

            _ => {}
        }
        Ok(())
    }
    // TODO: extract stack to separate struct

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
    let mut false_evaluator = False::new("1 1=[\"hello!\"]?");
    match false_evaluator.eval() {
        Ok(_) => {
            println!("Stack: {:?}", false_evaluator.stack);
            println!("Variables: {:?}", false_evaluator.variables)
        },
        Err(error) => eprintln!("{:?}", error)
    }
}
