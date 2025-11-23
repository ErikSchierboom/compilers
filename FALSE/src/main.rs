use std::ops::{Add, BitAnd, BitOr, Div, Mul, Neg, Not, Sub};

struct False {
    variables: [i32; 128],
    stack: Vec<i32>
}

impl False {


    fn new() -> Self {
        Self { variables: [0; 128], stack: Vec::new() }
    }

    fn eval(&mut self, source: &str) -> Option<i32> {
        macro_rules! unary {
            ($f: expr) => {{
                let a = self.stack.pop()?;
                self.stack.push($f(a))
            }}
        }

        macro_rules! binary {
            ($f: expr) => {{
                let b = self.stack.pop()?;
                let a = self.stack.pop()?;
                self.stack.push($f(a, b))
            }}
        }

        let mut ip: usize = 0;
        let bytes = source.as_bytes();

        while ip < bytes.len() {
            match bytes[ip] {
                b' ' | b'\t' | b'\n' | b'\r' => {},
                b'{' => {
                    while ip < bytes.len() && bytes[ip] != b'}' {
                        ip += 1
                    }
                }

                b'0' .. b'9' => {
                    let mut  n: i32 = 0;

                    while ip < bytes.len() && matches!(bytes[ip],  b'0' .. b'9')  {
                        n = n * 10 + (bytes[ip] - b'0') as i32;
                        ip += 1;
                    }

                    ip -= 1;
                    self.stack.push(n)
                },
                b'\'' => {
                    ip += 1;
                    self.stack.push(bytes[ip] as i32)
                },

                b'_' => unary!(i32::neg),
                b'~' => unary!(i32::not),

                b'&' => binary!(i32::bitand),
                b'|' => binary!(i32::bitor),
                b'=' => binary!(|a, b| if a == b { 1 } else { 0 }),
                b'<' => binary!(|a, b| if a < b { 1 } else { 0 }),
                b'>' => binary!(|a, b| if a > b { 1 } else { 0 }),
                b'+' => binary!(i32::add),
                b'-' => binary!(i32::sub),
                b'*' => binary!(i32::mul),
                b'/' => binary!(i32::div),

                b'%' => {
                    self.stack.pop()?;
                },
                b'$' => {
                    let a = self.stack.pop()?;
                    self.stack.push(a.clone());
                    self.stack.push(a);
                },
                b'\\' => {
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    self.stack.push(b);
                    self.stack.push(a);
                },
                b'@' => {
                    let c = self.stack.pop()?;
                    let b = self.stack.pop()?;
                    let a = self.stack.pop()?;
                    self.stack.push(c);
                    self.stack.push(a);
                    self.stack.push(b);
                },

                _ => {}
            }
            ip += 1;
        }

        self.stack.pop()
    }
}

fn main() {
    let mut false_evaluator = False::new();
    println!("{:?}", false_evaluator.eval("12 34 *"));
}
