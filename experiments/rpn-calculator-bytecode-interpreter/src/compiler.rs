use crate::parser::Expression;

pub enum Opcode {
    Constant,
    Add,
    Sub,
    Mul,
    Div
}

impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            0 => Opcode::Constant,
            1 => Opcode::Add,
            2 => Opcode::Sub,
            3 => Opcode::Mul,
            4 => Opcode::Div,
            _ => panic!("Unexpected opcode")
        }
    }
}

impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        value as u8
    }
}

pub struct Bytecode {
    pub op_codes: Vec<u8>,
    pub constants: Vec<i32>
}

impl Bytecode {
    pub fn new() -> Self {
        Self { op_codes: Vec::new(), constants: Vec::new() }
    }
}

pub struct Compiler {
    expressions: Vec<Expression>
}

impl Compiler {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Self { expressions }
    }

    pub fn compile(self) -> Bytecode {
        let mut bytecode = Bytecode::new();

        for expression in self.expressions {
            match expression {
                Expression::Integer(i) => {
                    bytecode.constants.push(i);
                    bytecode.op_codes.push(Opcode::Constant.into());
                    bytecode.op_codes.push((bytecode.constants.len() - 1) as u8);
                }
                Expression::Add => bytecode.op_codes.push(Opcode::Add.into()),
                Expression::Subtract => bytecode.op_codes.push(Opcode::Sub.into()),
                Expression::Multiply => bytecode.op_codes.push(Opcode::Mul.into()),
                Expression::Divide => bytecode.op_codes.push(Opcode::Div.into()),
            }
        }

        bytecode
    }
}