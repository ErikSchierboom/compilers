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
    fn execute(&self, environment: &mut Environment, span: &Span) -> RunResult {
        match self {
            Builtin::Add => add(environment, span),
            Builtin::Sub => sub(environment, span),
            Builtin::Mul => mul(environment, span),
            Builtin::Div => div(environment, span),
            Builtin::And => and(environment, span),
            Builtin::Or => or(environment, span),
            Builtin::Xor => xor(environment, span),
            Builtin::Not => not(environment, span),
            Builtin::Greater => greater(environment, span),
            Builtin::GreaterOrEqual => greater_or_equal(environment, span),
            Builtin::Less => less(environment, span),
            Builtin::LessOrEqual => less_or_equal(environment, span),
            Builtin::Equal => equal(environment, span),
            Builtin::NotEqual => not_equal(environment, span),
            Builtin::Concat => concat(environment, span),
            Builtin::Read => read(environment, span),
            Builtin::Write => write(environment, span),
            Builtin::Execute => execute(environment, span),
            Builtin::Print => print(environment, span),
            Builtin::Stack => stack(environment, span),
            Builtin::Dup => dup(environment, span),
            Builtin::Drop => drop(environment, span),
            Builtin::Swap => swap(environment, span),
            Builtin::Over => over(environment, span),
            Builtin::Nip => nip(environment, span),
            Builtin::When => when(environment, span),
            Builtin::Unless => unless(environment, span),
            Builtin::If => iff(environment, span),
            Builtin::Clear => clear(environment, span),
            Builtin::Rot => rot(environment, span),
            Builtin::Dip => dip(environment, span),
            Builtin::Keep => keep(environment, span),
            Builtin::Map => map(environment, span),
            Builtin::Filter => filter(environment, span),
            Builtin::Rem => rem(environment, span),
            Builtin::Max => max(environment, span),
            Builtin::Min => min(environment, span),
            Builtin::Fold => fold(environment, span),
            Builtin::Reduce => reduce(environment, span),
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn add(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_and_char_op(i64::add, f64::add, span) }
pub fn sub(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_and_char_op(i64::sub, f64::sub, span) }
pub fn mul(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_only_op(i64::mul, f64::mul, span) }
pub fn div(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_only_op(i64::div, f64::mul, span) }
pub fn and(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_int_only_op(i64::bitand, span) }
pub fn or(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_int_only_op(i64::bitor, span) }
pub fn xor(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_int_only_op(i64::bitxor, span) }
pub fn not(environment: &mut Environment, span: &Span) -> RunResult { environment.unary_int_only_op(i64::not, span) }
pub fn greater(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::gt, f64::gt, span) }
pub fn greater_or_equal(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::ge, f64::ge, span) }
pub fn less(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::lt, f64::lt, span) }
pub fn less_or_equal(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::le, f64::le, span) }
pub fn equal(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::eq, f64::eq, span) }
pub fn not_equal(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_compare_op(i64::ne, f64::ne, span) }
pub fn rem(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_and_char_op(i64::rem_euclid, f64::rem_euclid, span) }
pub fn max(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_and_char_op(i64::max, f64::max, span) }
pub fn min(environment: &mut Environment, span: &Span) -> RunResult { environment.binary_number_and_char_op(i64::min, f64::min, span) }

pub fn print(environment: &mut Environment, span: &Span) -> RunResult {
    let value = environment.pop(&span)?;
    println!("{value}");
    Ok(())
}

pub fn stack(environment: &mut Environment, span: &Span) -> RunResult {
    while let Ok(value) = environment.pop(span) {
        println!("{value}")
    }
    Ok(())
}

pub fn read(environment: &mut Environment, span: &Span) -> RunResult {
    match environment.pop(span)? {
        Value::ValQuotedWord(name) => {
            let variable = environment.get_variable(&name, span)?;
            environment.push(variable.clone());
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuotedWord, span.clone()))
    }
}

pub fn write(environment: &mut Environment, span: &Span) -> RunResult {
    match environment.pop(span)? {
        Value::ValQuotedWord(name) => {
            let value = environment.pop(span)?;
            environment.set_variable(name, value);
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::ExpectedQuotedWord, span.clone()))
    }
}

pub fn execute(environment: &mut Environment, span: &Span) -> RunResult {
    let value = match environment.pop(span)? {
        Value::ValQuotedWord(name) => environment.get_variable(&name, &span)?,
        Value::ValBuiltin(builtin) => Value::ValBuiltin(builtin),
        Value::ValBlock(words) => Value::ValBlock(words),
        _ => return Err(Spanned::new(RuntimeError::NonExecutableWord, span.clone()))
    };
    environment.execute(value, span)
}

pub fn dup(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    environment.push(top.clone());
    environment.push(top);
    Ok(())
}

pub fn drop(environment: &mut Environment, span: &Span) -> RunResult {
    environment.pop(span)?;
    Ok(())
}

pub fn swap(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    environment.push(top);
    environment.push(snd);
    Ok(())
}

pub fn over(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    environment.push(snd.clone());
    environment.push(top);
    environment.push(snd);
    Ok(())
}

pub fn nip(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    environment.pop(span)?;
    environment.push(top);
    Ok(())
}

pub fn clear(environment: &mut Environment, _span: &Span) -> RunResult {
    environment.stack.borrow_mut().clear();
    Ok(())
}

pub fn when(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    if bool::from(snd) {
        environment.execute(top, span)?
    }

    Ok(())
}

pub fn unless(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    if !bool::from(snd) {
        environment.execute(top, span)?
    }

    Ok(())
}

pub fn iff(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    let third = environment.pop(span)?;

    if bool::from(third) {
        environment.execute(snd, span)?
    } else {
        environment.execute(top, span)?
    }

    Ok(())
}

pub fn rot(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    let third = environment.pop(span)?;

    environment.push(top);
    environment.push(third);
    environment.push(snd);
    Ok(())
}

pub fn dip(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    environment.execute(top, span)?;
    environment.push(snd);
    Ok(())
}

pub fn keep(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    environment.push(snd.clone());
    environment.execute(top, span)?;
    environment.push(snd);
    Ok(())
}

pub fn map(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut mapped_array = Vec::new();

            for element in array {
                let stack_length_before = environment.stack.borrow().len();
                environment.push(element);
                environment.execute(top.clone(), span)?;

                let stack_len = environment.stack.borrow().len();

                if stack_len < stack_length_before {
                    return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone()));
                }

                match stack_len - stack_length_before {
                    0 => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    1 => mapped_array.push(environment.pop(span)?),
                    _ => mapped_array.push(Value::ValArray(environment.stack.borrow_mut().drain(stack_length_before..).collect())),
                }
            }

            environment.push(Value::ValArray(mapped_array));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn filter(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    match snd {
        Value::ValArray(array) => {
            let mut filtered_array = Vec::new();

            for element in array {
                let stack_length_before = environment.stack.borrow().len();
                environment.push(element.clone());
                environment.execute(top.clone(), span)?;

                let stack_len = environment.stack.borrow().len();

                match stack_len.cmp(&stack_length_before) {
                    Ordering::Less => return Err(Spanned::new(RuntimeError::WordHasNegativeStackEffect, span.clone())),
                    Ordering::Equal => return Err(Spanned::new(RuntimeError::WordDoesNotHavePositiveStackEffect, span.clone())),
                    Ordering::Greater => {
                        if environment.stack.borrow().len() == stack_length_before + 1 {
                            if bool::from(environment.pop(span)?) {
                                filtered_array.push(element)
                            }
                        }
                    }
                }
            }

            environment.push(Value::ValArray(filtered_array));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn reduce(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    match snd {
        Value::ValArray(mut array) => {
            match array.split_first_mut() {
                Some((head, tail)) => {
                    environment.push(head.to_owned());

                    for element in tail.to_owned() {
                        environment.push(element);
                        environment.execute(top.clone(), span)?;
                    }

                    Ok(())
                }
                None => Err(Spanned::new(RuntimeError::EmptyArray, span.clone())),
            }
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone())),
    }
}

pub fn fold(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;
    let third = environment.pop(span)?;

    match third {
        Value::ValArray(array) => {
            environment.push(snd);

            for element in array {
                environment.push(element);
                environment.execute(top.clone(), span)?;
            }

            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}

pub fn concat(environment: &mut Environment, span: &Span) -> RunResult {
    let top = environment.pop(span)?;
    let snd = environment.pop(span)?;

    match (snd, top) {
        (Value::ValArray(mut l), Value::ValArray(mut r)) => {
            l.append(&mut r);
            environment.push(Value::ValArray(l));
            Ok(())
        }
        (Value::ValBlock(mut l), Value::ValBlock(mut r)) => {
            l.append(&mut r);
            environment.push(Value::ValBlock(l));
            Ok(())
        }
        (Value::ValString(mut l), Value::ValString(r)) => {
            l.push_str(&r);
            environment.push(Value::ValString(l));
            Ok(())
        }
        _ => Err(Spanned::new(RuntimeError::UnsupportedOperands, span.clone()))
    }
}
