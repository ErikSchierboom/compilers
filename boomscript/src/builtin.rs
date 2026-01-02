use crate::interpreter::{Executable, Interpreter, RunResult, RuntimeError, Value};
use crate::location::Span;
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
    let (name, location) = match interpreter.pop(span)? {
        Value::ValQuotedWord(name) => (name, location),
        value => return Err(RuntimeError::ExpectedQuote(value.location().clone()))
    };
    let variable = interpreter.get_variable(&name)?;
    interpreter.push(variable.clone());
    Ok(())
}

pub fn write(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let name = match interpreter.pop(span)? {
        Value::ValQuotedWord(name, _) => name,
        value => return Err(RuntimeError::ExpectedQuote(value.location().clone()))
    };
    let value = interpreter.pop(span)?;
    interpreter.set_variable(name, value);
    Ok(())
}

pub fn execute(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let value = match interpreter.pop(span)? {
        Value::ValQuotedWord(name, location) => interpreter.get_variable(&name)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words, location) => Value::ValBlock(words, location),
        value => return Err(RuntimeError::ExpectedExecutableWord(value.location().clone()))
    };
    interpreter.execute(value)
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

pub fn clear(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    interpreter.stack.clear();
    Ok(())
}

pub fn when(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if bool::from(snd.value) {
        interpreter.execute(top)?
    }

    Ok(())
}

pub fn unless(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    if !bool::from(snd.value) {
        interpreter.execute(top)?
    }

    Ok(())
}

pub fn iff(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    if bool::from(third.value) {
        interpreter.execute(snd)?
    } else {
        interpreter.execute(top)?
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
    interpreter.execute(top)?;
    interpreter.push(snd);
    Ok(())
}

pub fn keep(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    interpreter.push(snd.clone());
    interpreter.execute(top)?;
    interpreter.push(snd);
    Ok(())
}

pub fn map(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array, location) => {
            let mut mapped_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element);
                interpreter.execute(top.clone())?;

                if interpreter.stack.len() < stack_length_before {
                    return Err(RuntimeError::WordHasNegativeStackEffect(location.clone()));
                }

                match interpreter.stack.len() - stack_length_before {
                    0 => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect(location.clone())),
                    1 => mapped_array.push(interpreter.pop(span)?),
                    _ => mapped_array.push(Value::ValArray(interpreter.stack.drain(stack_length_before..).collect(), location)),
                }
            }

            interpreter.push(Value::ValArray(mapped_array, location))
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

pub fn filter(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(array, location) => {
            let mut filtered_array = Vec::new();

            for element in array {
                let stack_length_before = interpreter.stack.len();
                interpreter.push(element.clone());
                interpreter.execute(top.clone())?;

                match interpreter.stack.len().cmp(&stack_length_before) {
                    Ordering::Less => return Err(RuntimeError::WordHasNegativeStackEffect(location)),
                    Ordering::Equal => return Err(RuntimeError::WordDoesNotHavePositiveStackEffect(location)),
                    Ordering::Greater => {
                        if interpreter.stack.len() == stack_length_before + 1 {
                            if bool::from(interpreter.pop(span)?) {
                                filtered_array.push(element)
                            }
                        }
                    }
                }
            }

            interpreter.push(Value::ValArray(filtered_array, location))
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

pub fn reduce(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match snd {
        Value::ValArray(mut array, location) => {
            match array.split_first_mut() {
                Some((head, tail)) => {
                    interpreter.push(head.to_owned());

                    for element in tail.to_owned() {
                        interpreter.push(element);
                        interpreter.execute(top.clone())?;
                    }
                }
                None => return Err(RuntimeError::EmptyArray(location)),
            }
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

pub fn fold(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;
    let third = interpreter.pop(span)?;

    match third {
        Value::ValArray(array, _) => {
            interpreter.push(snd);

            for element in array {
                interpreter.push(element);
                interpreter.execute(top.clone())?;
            }
        }
        value => return Err(RuntimeError::UnsupportedOperands(value.location().clone()))
    }

    Ok(())
}

pub fn concat(interpreter: &mut Interpreter, span: &Span) -> RunResult {
    let top = interpreter.pop(span)?;
    let snd = interpreter.pop(span)?;

    match (snd, top) {
        (Value::ValArray(mut l, location_l), Value::ValArray(mut r, location_r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValArray(l, location_l.merge(&location_r)));
        }
        (Value::ValBlock(mut l, location_l), Value::ValBlock(mut r, location_r)) => {
            l.append(&mut r);
            interpreter.push(Value::ValBlock(l, location_l.merge(&location_r)));
        }
        (Value::ValString(mut l, location_l), Value::ValString(r, location_r)) => {
            l.push_str(&r);
            interpreter.push(Value::ValString(l, location_l.merge(&location_r)));
        }
        (l, r) => return Err(RuntimeError::UnsupportedOperands(l.location().merge(r.location())))
    }

    Ok(())
}
