use crate::interpreter::{Executable, Interpreter, RunResult, RuntimeError, Value};
use crate::location::{Span, Spanned};
use std::cmp::Ordering;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Sub};

#[derive(Clone, Debug)]
pub struct Builtin(pub fn(&mut Interpreter, span: &Span) -> RunResult);

impl Executable for Builtin {
    fn execute(&self, interpreter: &mut Interpreter, span: &Span) -> RunResult {
        self.0(interpreter, span)
    }
}

pub fn add(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::add, f64::add, span) }
pub fn sub(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::sub, f64::sub, span) }
pub fn mul(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_only_op(i64::mul, f64::mul, span) }
pub fn div(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_only_op(i64::div, f64::mul, span) }
pub fn and(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitand, span) }
pub fn or(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitor, span) }
pub fn xor(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_int_only_op(i64::bitxor, span) }
pub fn not(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.unary_int_only_op(i64::not, span) }
pub fn greater(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::gt, f64::gt, span) }
pub fn greater_or_equal(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::ge, f64::ge, span) }
pub fn less(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::lt, f64::lt, span) }
pub fn less_or_equal(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::le, f64::le, span) }
pub fn equal(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::eq, f64::eq, span) }
pub fn not_equal(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_compare_op(i64::ne, f64::ne, span) }
pub fn rem(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::rem_euclid, f64::rem_euclid, span) }
pub fn max(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::max, f64::max, span) }
pub fn min(interpreter: &mut Interpreter, span: &Span) -> RunResult { interpreter.binary_number_and_char_op(i64::min, f64::min, span) }

pub fn read(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => {
            let variable = interpreter.get_variable(&name, span)?;
            interpreter.push(variable.clone());
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuote, span.clone()))
    }
}

pub fn write(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => {
            let value = interpreter.pop(span)?;
            interpreter.set_variable(name, value);
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuote, span.clone()))
    }
}

pub fn execute(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let value = match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => interpreter.get_variable(&name, &span)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words) => Value::ValBlock(words),
        _ => return Err(Spanned::new(RuntimeError::ExpectedExecutableWord, span.clone()))
    };
    interpreter.execute(value, span)
}

pub fn dup(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    interpreter.push(top.clone());
    interpreter.push(top);
    Ok(())
}

pub fn drop(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    interpreter.pop(span)?;
    Ok(())
}

pub fn swap(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

pub fn over(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(snd.clone());
    interpreter.push(top);
    interpreter.push(snd);
    Ok(())
}

pub fn nip(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    interpreter.pop(span)?;
    interpreter.push(top);
    Ok(())
}

pub fn clear(interpreter: &mut Interpreter, _span: &Span) -> RunResult {
    interpreter.stack.clear();
    Ok(())
}

pub fn when(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if bool::from(snd) {
        interpreter.execute(top, span)?
    }

    Ok(())
}

pub fn unless(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if !bool::from(snd) {
        interpreter.execute(top, span)?
    }

    Ok(())
}

pub fn iff(interpreter: &mut Interpreter, span: &Span) -> RunResult {
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

pub fn rot(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    interpreter.push(top);
    interpreter.push(third);
    interpreter.push(snd);
    Ok(())
}

pub fn dip(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.execute(top, span)?;
    interpreter.push(snd);
    Ok(())
}

pub fn keep(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(snd.clone());
    interpreter.execute(top, span)?;
    interpreter.push(snd);
    Ok(())
}

pub fn map(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut mapped_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element);
                interpreter.execute(top.clone(), span)?;

                if interpreter.stack.len() < stack_length_before {
                    return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone()));
                }

                match interpreter.stack.len() - stack_length_before {
                    0 => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    1 => mapped_array.push(interpreter.pop(span)?),
                    _ => mapped_array.push(Value::ValArray(interpreter.stack.drain(stack_length_before..).collect())),
                }
            }

            interpreter.push(Value::ValArray(mapped_array));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn filter(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut filtered_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element.clone());
                interpreter.execute(top.clone(), span)?;

                match interpreter.stack.len().cmp(&stack_length_before) {
                    Ordering::Less => return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone())),
                    Ordering::Equal => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    Ordering::Greater => {
                        if interpreter.stack.len() == stack_length_before + 1 {
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

pub fn reduce(interpreter: &mut Interpreter, span: &Span) -> RunResult {
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

pub fn fold(interpreter: &mut Interpreter, span: &Span) -> RunResult {
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

pub fn concat(interpreter: &mut Interpreter, span: &Span) -> RunResult {
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
