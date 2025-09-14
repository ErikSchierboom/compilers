use crate::location::Spanned;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Shape {
    Scalar,
    Linear(u16),
    Matrix(u16, u16)
}

#[derive(Clone, Debug)]
pub enum Scalar {
    Integer(i64),
}

impl Scalar {
    pub fn as_integer(&self) -> Option<&i64> {
        match self {
            Scalar::Integer(i) => Some(i),
            _ => None
        }
    }
}

impl Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scalar::Integer(i) => write!(f, "{i}")
        }
    }
}

#[derive(Clone, Debug)]
pub struct Array {
    pub shape: Shape,
    pub values: Vec<Spanned<Scalar>>,
}

impl Array {
    pub fn new(shape: Shape, values: Vec<Spanned<Scalar>>) -> Self {
        Array { shape, values }
    }

    pub fn scalar(element: Spanned<Scalar>) -> Self {
        Self::new(Shape::Scalar, vec![element])
    }

    pub fn linear(elements: Vec<Spanned<Scalar>>) -> Self {
        Self::new(Shape::Linear(elements.len() as u16), elements)
    }

    pub fn matrix(elements: Vec<Vec<Spanned<Scalar>>>) -> Self {
        let num_rows = elements.len() as u16;
        let num_columns = elements.get(0).map(|x| x.len()).unwrap_or(0) as u16;
        let elements = elements.into_iter().flatten().collect();
        Self::new(Shape::Matrix(num_columns, num_rows), elements)
    }
    
    // TODO: add error handling
    pub fn as_integers(&self) -> Vec<&i64> {
        self.values.iter().map(|spanned_value| spanned_value.value.as_integer().unwrap()).collect()
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.shape {
            Shape::Scalar => write!(f, "{}", self.values.first().unwrap().value),
            Shape::Linear(num_elements) => {
                write!(f, "[")?;
                for (column, value) in self.values.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if (column as u16) < num_elements - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            Shape::Matrix(num_columns, num_rows) => {
                let column_widths: Vec<usize> = (0..num_columns).map(|column|
                         (0..num_rows).map(|row|
                            self.values.get((row * num_columns + column) as usize).unwrap().to_string().len()
                     ).max().unwrap_or_default())
                    .collect();

                write!(f, "[")?;

                for row in 0..num_rows {
                    if row > 0 {
                        write!(f, " ")?;
                    }

                    for column in 0..num_columns {
                        let column_width = column_widths[column];
                        let value = self.values.get((row * num_columns + column) as usize).unwrap();
                        write!(f, "{:>column_width$}", value)?;
                        if column < num_columns - 1 {
                            write!(f, " ")?;
                        }
                    }

                    if row < num_rows - 1 {
                        write!(f, "\n")?;
                    }
                }
          
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::location::Span;

    impl Into<Spanned<Scalar>> for i64 {
        fn into(self) -> Spanned<Scalar> {
            Spanned::new(Scalar::Integer(self), Span::EMPTY)
        }
    }

    #[test]
    fn test_display_scalar() {
        let scalar = Array::scalar(5.into());
        assert_eq!(scalar.to_string(), "5");
    }

    #[test]
    fn test_display_linear() {
        let empty = Array::linear(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single = Array::linear(vec![13.into()]);
        assert_eq!(single.to_string(), "[13]");

        let multiples = Array::linear(vec![27.into(), 9.into(), 1.into()]);
        assert_eq!(multiples.to_string(), "[27 9 1]");
    }

    #[test]
    fn test_display_matrix() {
        let empty = Array::matrix(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single_row_no_columns = Array::matrix(vec![vec![]]);
        assert_eq!(single_row_no_columns.to_string(), "[]");

        let single_row_one_column = Array::matrix(vec![vec![2.into()]]);
        assert_eq!(single_row_one_column.to_string(), "[2]");

        let multiple_rows_one_column = Array::matrix(
        vec![
                    vec![2.into()], 
                    vec![3.into()],
                    vec![4.into()]]);
        assert_eq!(multiple_rows_one_column.to_string(),
                   concat!("[2\n",
                   " 3\n",
                   " 4]"));

        let multiples_rows_multiple_columns = Array::matrix(
        vec![
                    vec![1.into(), 2.into(), 3.into()],
                    vec![4.into(), 5.into(), 6.into()],
                    vec![7.into(), 8.into(), 9.into()]]);
        assert_eq!(multiples_rows_multiple_columns.to_string(),
                   concat!("[1 2 3\n",
                   " 4 5 6\n",
                   " 7 8 9]"));

        let column_values_are_padded = Array::matrix(
        vec![
                    vec![1.into(), 222.into(), 3.into()],
                    vec![44.into(), 55.into(), 6.into()],
                    vec![7.into(), 8.into(), 90.into()]]);
        assert_eq!(column_values_are_padded.to_string(),
                   concat!("[ 1 222  3\n",
                   " 44  55  6\n",
                   "  7   8 90]"));
    }
}