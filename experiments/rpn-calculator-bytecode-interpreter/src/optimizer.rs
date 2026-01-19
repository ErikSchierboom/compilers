use crate::parser::Expression;

pub fn optimize(expressions: Vec<Expression>) -> Vec<Expression> {
    let mut expressions = expressions;

    while let Some(position) = expressions.as_slice().windows(2).position(|slice|
        matches!(slice, [Expression::Integer(0), Expression::Add]) ||
        matches!(slice, [Expression::Integer(0), Expression::Subtract])) {
        expressions.drain(position..position + 2);
    }

    while let Some(position) = expressions.as_slice().windows(3).position(|slice|
        matches!(slice, [Expression::Integer(_), Expression::Integer(0), Expression::Multiply])) {
        expressions.drain(position..position + 3);
        expressions.insert(position, Expression::Integer(0));
    }

    expressions
}