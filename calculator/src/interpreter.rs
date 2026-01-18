use crate::parser::{parse, BinaryOperator, Expression, ParseError, UnaryOperator};

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError)
}

fn evaluate_expression(expression: &Expression) -> Result<i64, RuntimeError> {
    match expression {
        Expression::Number(i) => Ok(i.clone()),
        Expression::Unary(right, UnaryOperator::Neg) => Ok(-evaluate_expression(right.as_ref())?),
        Expression::Unary(right, UnaryOperator::Pos) => Ok(evaluate_expression(right.as_ref())?),
        Expression::Binary(left, BinaryOperator::Add, right) => Ok(evaluate_expression(left.as_ref())? + evaluate_expression(right.as_ref())?),
        Expression::Binary(left, BinaryOperator::Sub, right) => Ok(evaluate_expression(left.as_ref())? - evaluate_expression(right.as_ref())?),
        Expression::Binary(left, BinaryOperator::Mul, right) => Ok(evaluate_expression(left.as_ref())? * evaluate_expression(right.as_ref())?),
        Expression::Binary(left, BinaryOperator::Div, right) => Ok(evaluate_expression(left.as_ref())? / evaluate_expression(right.as_ref())?),
        Expression::Grouping(inner) => evaluate_expression(inner)
    }
}

pub fn evaluate(code: &str) -> Result<i64, RuntimeError> {
    match parse(code) {
        Ok(expression) => evaluate_expression(&expression),
        Err(error) => Err(RuntimeError::Parse(error)),
    }
}
