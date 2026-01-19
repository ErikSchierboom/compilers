use crate::compiler::{Bytecode, Opcode};

pub struct Interpreter {
    stack: Vec<i32>
}

impl Interpreter {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn interpret(&mut self, bytecode: Bytecode) -> Vec<i32> {
        let mut iter = bytecode.op_codes.into_iter();

        while let Some(byte_code) = iter.next() {
            let op_code: Opcode = byte_code.into();

            match op_code {
                Opcode::Constant => {
                    let constant_index = iter.next().unwrap();
                    let constant = bytecode.constants[constant_index as usize];
                    self.stack.push(constant);
                }
                Opcode::Add => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(left + right);
                }
                Opcode::Sub => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(left - right);
                }
                Opcode::Mul => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(left * right);
                }
                Opcode::Div => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();
                    self.stack.push(left / right);
                }
            }
        }

        self.stack.clone()
    }
}
