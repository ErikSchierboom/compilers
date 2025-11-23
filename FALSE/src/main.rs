use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Sub};

#[derive(Debug)]
enum FalseError {
    EmptyStack
}

struct False {
    variables: [i32; 26],
    stack: Vec<i32>
}

impl False {
    fn new() -> Self {
        Self { variables: [0; 26], stack: Vec::new() }
    }

    fn eval(&mut self, source: &str) -> Result<(), FalseError> {
        macro_rules! unary {
            ($f: expr) => {{
                let a = self.pop()?;
                self.push($f(a));
            }}
        }

        macro_rules! binary {
            ($f: expr) => {{
                let b = self.pop()?;
                let a = self.pop()?;
                self.push($f(a, b));
            }}
        }

        let mut ip: usize = 0;
        let chars: Vec<char> = source.chars().collect();

        while ip < chars.len() {
            match chars[ip] as char {
                ' ' | '\t' | '\n' | '\r' => {},
                '{' => {
                    while ip < chars.len() && chars[ip] != '}' {
                        ip += 1
                    }
                }

                '0'..='9' => {
                    let mut  n: i32 = 0;

                    while ip < chars.len() && matches!(chars[ip],  '0'..='9')  {
                        n = n * 10 + (chars[ip] as u8 - b'0') as i32;
                        ip += 1;
                    }

                    ip -= 1;
                    self.push(n)
                },
                '\'' => {
                    ip += 1;
                    self.push(chars[ip] as i32)
                },

                '_' => unary!(i32::neg),
                '~' => unary!(i32::not),

                '&' => binary!(i32::bitand),
                '|' => binary!(i32::bitor),
                '=' => binary!(|a, b| if a == b { -1 } else { 0 }),
                '<' => binary!(|a, b| if a < b { -1 } else { 0 }),
                '>' => binary!(|a, b| if a > b { -1 } else { 0 }),
                '+' => binary!(i32::add),
                '-' => binary!(i32::sub),
                '*' => binary!(i32::mul),
                '/' => binary!(i32::div),

                'a'..='z' => self.push((chars[ip] as u8 - b'a') as i32),
                'A'..='Z' => self.push(self.variables[(chars[ip] as u8 - b'A') as usize]),

                ':' => {
                    let var_addr = self.pop()?;
                    let value = self.pop()?;
                    self.variables[var_addr as usize] = value
                },
                ';' => {
                    let var_addr = self.pop()?;
                    self.push(self.variables[var_addr as usize]);
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
                    let n = self.pop()?;
                    let nth_val = self.pick(n as usize)?.clone();
                    self.push(nth_val)
                }

                '.' => print!("{}", self.peek().unwrap()),
                ',' => print!("{}", *self.peek().unwrap() as u8 as char),
                '"' => {
                    ip += 1;
                    while ip < chars.len() && chars[ip] != '"' {
                        print!("{}", chars[ip] as u8 as char);
                        ip += 1;
                    }
                }

                _ => {}
            }
            ip += 1;
        }

        Ok(())
    }

    fn push(&mut self, value: i32) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Result<i32, FalseError> {
        self.stack.pop().ok_or(FalseError::EmptyStack)
    }

    fn peek(&mut self) -> Result<&i32, FalseError> {
        self.stack.last().ok_or(FalseError::EmptyStack)
    }

    fn pick(&mut self, n: usize) -> Result<&i32, FalseError> {
        self.stack.iter().nth_back(n).ok_or(FalseError::EmptyStack)
    }
}

fn main() {
    let mut false_evaluator = False::new();
    match false_evaluator.eval("7 8 9 2 ø") {
        Ok(_) => println!("{:?}", false_evaluator.stack),
        Err(error) => eprintln!("{:?}", error)
    }
}
