use crate::interpreter::{Environment, Executable, RunResult, RuntimeError, Value};
use crate::location::{Span, Spanned};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub};

#[derive(Clone, Debug)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Not,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equal,
    NotEqual,
    Concat,
    Read,
    Write,
    Execute,
    Print,
    Stack,
    Dup,
    Drop,
    Swap,
    Over,
    Nip,
    When,
    Unless,
    If,
    Clear,
    Rot,
    Dip,
    Keep,
    Map,
    Filter,
    Rem,
    Max,
    Min,
    Fold,
    Reduce,
}

impl Executable for Builtin {
    fn execute(&self, interpreter: &mut Environment, span: &Span) -> RunResult {
        match self {
            Builtin::Add => add(interpreter, span),
            Builtin::Sub => sub(interpreter, span),
            Builtin::Mul => mul(interpreter, span),
            Builtin::Div => div(interpreter, span),
            Builtin::And => and(interpreter, span),
            Builtin::Or => or(interpreter, span),
            Builtin::Xor => xor(interpreter, span),
            Builtin::Not => not(interpreter, span),
            Builtin::Greater => greater(interpreter, span),
            Builtin::GreaterOrEqual => greater_or_equal(interpreter, span),
            Builtin::Less => less(interpreter, span),
            Builtin::LessOrEqual => less_or_equal(interpreter, span),
            Builtin::Equal => equal(interpreter, span),
            Builtin::NotEqual => not_equal(interpreter, span),
            Builtin::Concat => concat(interpreter, span),
            Builtin::Read => read(interpreter, span),
            Builtin::Write => write(interpreter, span),
            Builtin::Execute => execute(interpreter, span),
            Builtin::Print => print(interpreter, span),
            Builtin::Stack => stack(interpreter, span),
            Builtin::Dup => dup(interpreter, span),
            Builtin::Drop => drop(interpreter, span),
            Builtin::Swap => swap(interpreter, span),
            Builtin::Over => over(interpreter, span),
            Builtin::Nip => nip(interpreter, span),
            Builtin::When => when(interpreter, span),
            Builtin::Unless => unless(interpreter, span),
            Builtin::If => iff(interpreter, span),
            Builtin::Clear => clear(interpreter, span),
            Builtin::Rot => rot(interpreter, span),
            Builtin::Dip => dip(interpreter, span),
            Builtin::Keep => keep(interpreter, span),
            Builtin::Map => map(interpreter, span),
            Builtin::Filter => filter(interpreter, span),
            Builtin::Rem => rem(interpreter, span),
            Builtin::Max => max(interpreter, span),
            Builtin::Min => min(interpreter, span),
            Builtin::Fold => fold(interpreter, span),
            Builtin::Reduce => reduce(interpreter, span),
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn add(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::add, f64::add, span) }
pub fn sub(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::sub, f64::sub, span) }
pub fn mul(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_only_op(i64::mul, f64::mul, span) }
pub fn div(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_only_op(i64::div, f64::mul, span) }
pub fn and(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitand, span) }
pub fn or(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitor, span) }
pub fn xor(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitxor, span) }
pub fn not(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.unary_int_only_op(i64::not, span) }
pub fn greater(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::gt, f64::gt, span) }
pub fn greater_or_equal(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::ge, f64::ge, span) }
pub fn less(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::lt, f64::lt, span) }
pub fn less_or_equal(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::le, f64::le, span) }
pub fn equal(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::eq, f64::eq, span) }
pub fn not_equal(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::ne, f64::ne, span) }
pub fn rem(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::rem_euclid, f64::rem_euclid, span) }
pub fn max(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::max, f64::max, span) }
pub fn min(interpreter: &mut Environment, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::min, f64::min, span) }

pub fn print(interpreter: &mut Environment, span: &Span) -> RunResult {
    let value = interpreter.pop(&span)?;
    println!("{value}");
    Ok(())
}

pub fn stack(interpreter: &mut Environment, span: &Span) -> RunResult {
    while let Ok(value) = interpreter.pop(span) {
        println!("{value}")
    }
    Ok(())
}

pub fn read(interpreter: &mut Environment, span: &Span) -> RunResult {
    match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => {
            let variable = interpreter.get_variable(&name, span)?;
            interpreter.push(variable.clone());
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuotedWord, span.clone()))
    }
}

pub fn write(interpreter: &mut Environment, span: &Span) -> RunResult {
    match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => {
            let value = interpreter.pop(span)?;
            interpreter.set_variable(name, value);
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuotedWord, span.clone()))
    }
}

pub fn execute(interpreter: &mut Environment, span: &Span) -> RunResult {
    let value = match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => interpreter.get_variable(&name, &span)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words) => Value::ValBlock(words),
        _ => return Err(Spanned::new(RuntimeError::NonExecutableWord, span.clone()))
    };
    interpreter.execute(value, span)
}

pub fn dup(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    interpreter.push(top.clone());
    interpreter.push(top);
    Ok(())
}

pub fn drop(interpreter: &mut Environment, span: &Span) -> RunResult {
    interpreter.pop(span)?;
    Ok(())
}

pub fn swap(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

pub fn over(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(snd.clone());
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

pub fn nip(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    interpreter.pop(span)?;
    interpreter.push(top);
    Ok(())
}

pub fn clear(interpreter: &mut Environment, _span: &Span) -> RunResult {
    interpreter.stack.borrow_mut().clear();
    Ok(())
}

pub fn when(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if bool::from(snd) {
        interpreter.execute(top, span)?
    }

    Ok(())
}

pub fn unless(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if !bool::from(snd) {
        interpreter.execute(top, span)?
    }

    Ok(())
}

pub fn iff(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    if bool::from(third) {
        interpreter.execute(snd, span)?
    } else {
        interpreter.execute(top, span)?
    }

    Ok(())
}

pub fn rot(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    interpreter.push(top);
    interpreter.push(third);
    interpreter.push(snd);
    Ok(())
}

pub fn dip(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.execute(top, span)?;
    interpreter.push(snd);
    Ok(())
}

pub fn keep(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(snd.clone());
    interpreter.execute(top, span)?;
    interpreter.push(snd);
    Ok(())
}

pub fn map(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut mapped_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.borrow().len();
                interpreter.push(element);
                interpreter.execute(top.clone(), span)?;

                let stack_len = interpreter.stack.borrow().len();

                if stack_len < stack_length_before {
                    return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone()));
                }

                match stack_len - stack_length_before {
                    0 => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    1 => mapped_array.push(interpreter.pop(span)?),
                    _ => mapped_array.push(Value::ValArray(interpreter.stack.borrow_mut().drain(stack_length_before..).collect())),
                }
            }

            interpreter.push(Value::ValArray(mapped_array));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn filter(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut filtered_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.borrow().len();
                interpreter.push(element.clone());
                interpreter.execute(top.clone(), span)?;

                let stack_len = interpreter.stack.borrow().len();

                match stack_len.cmp(&stack_length_before) {
                    Ordering::Less => return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone())),
                    Ordering::Equal => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    Ordering::Greater => {
                        if interpreter.stack.borrow().len() == stack_length_before + 1 {
                            if bool::from(interpreter.pop(span)?) {
                                filtered_array.push(element)
                            }
                        }
                    }
                }
            }

            interpreter.push(Value::ValArray(filtered_array));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn reduce(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(mut array) => {
            match array.split_first_mut() {
                Some((head, tail)) => {
                    interpreter.push(head.to_owned());

                    for element in tail.to_owned() {
                        interpreter.push(element);
                        interpreter.execute(top.clone(), span)?;
                    }

                    Ok(())
                }
                None => Err(Spanned::new(RuntimeError::EmptyArray, span.clone())),
            }
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone())),
    }
}

pub fn fold(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    match third {
        Value::ValArray(array) => {
            interpreter.push(snd);

            for element in array {
                interpreter.push(element);
                interpreter.execute(top.clone(), span)?;
            }

            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn concat(interpreter: &mut Environment, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match (snd, top) {
        (Value::ValArray(mut l), Value::ValArray(mut r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValArray(l));
            Ok(())
        }
        (Value::ValBlock(mut l), Value::ValBlock(mut r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValBlock(l));
            Ok(())
        }
        (Value::ValString(mut l), Value::ValString(r)) => {
            l.push_str(&r);
            interpreter.push(Value::ValString(l));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}
