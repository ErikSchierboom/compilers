use crate::parser::{parse, BinaryOperator, Expression, ParseError, UnaryOperator};

#[derive(Debug)]
pub enum RuntimeError {
    Parse(ParseError)
}

// TODO: add trait

pub struct Interpreter {
    expressions: Vec<Expression>,
}

impl Interpreter {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Self { expressions }
    }

    pub fn evaluate(&self) -> Result<Vec<i64>, RuntimeError> {
        self.expressions.iter().map(|expression| self.evaluate_expression(&expression)).collect()
    }

    pub fn evaluate_expression(&self, expression: &Expression) -> Result<i64, RuntimeError> {
        match expression {
            Expression::Number(i) => Ok(i.clone()),
            Expression::Unary(right, UnaryOperator::Neg) => Ok(-self.evaluate_expression(right.as_ref())?),
            Expression::Unary(right, UnaryOperator::Pos) => Ok(self.evaluate_expression(right.as_ref())?),
            Expression::Binary(left, BinaryOperator::Add, right) => Ok(self.evaluate_expression(left.as_ref())? + self.evaluate_expression(right.as_ref())?),
            Expression::Binary(left, BinaryOperator::Sub, right) => Ok(self.evaluate_expression(left.as_ref())? - self.evaluate_expression(right.as_ref())?),
            Expression::Binary(left, BinaryOperator::Mul, right) => Ok(self.evaluate_expression(left.as_ref())? * self.evaluate_expression(right.as_ref())?),
            Expression::Binary(left, BinaryOperator::Div, right) => Ok(self.evaluate_expression(left.as_ref())? / self.evaluate_expression(right.as_ref())?),
            Expression::Grouping(inner) => self.evaluate_expression(inner)
        }
    }
}

pub fn evaluate(code: &str) -> Result<Vec<i64>, RuntimeError> {
    match parse(code) {
        Ok(expression) => Interpreter::new(vec![expression]).evaluate(),
        Err(error) => Err(RuntimeError::Parse(error)),
    }
}
