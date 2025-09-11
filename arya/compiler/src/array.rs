use crate::location::Spanned;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Scalar {
    Integer(i64),
}

impl Display for Scalar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scalar::Integer(i) => write!(f, "{i}")
        }
    }
}

#[derive(Clone, Debug)]
pub enum Array {
    Scalar(Spanned<Scalar>),
    Linear(Linear),
    Matrix(Matrix),
}

#[derive(Clone, Debug)]
pub struct Linear {
    pub values: Vec<Spanned<Scalar>>,
    pub len: u16,
}

#[derive(Clone, Debug)]
pub struct Matrix {
    pub values: Vec<Vec<Spanned<Scalar>>>,
    pub rows: u16,
    pub columns: u16,
}

impl Array {
    pub fn scalar(element: Spanned<Scalar>) -> Self {
        Array::Scalar(element)
    }

    pub fn linear(elements: Vec<Spanned<Scalar>>) -> Self {
        let length = elements.len() as u16;
        Array::Linear(Linear { values: elements, len: length })
    }

    pub fn matrix(elements: Vec<Vec<Spanned<Scalar>>>) -> Self {
        let num_rows = elements.len() as u16;
        let num_columns = elements.get(0).map(|x| x.len()).unwrap_or(0) as u16;
        Array::Matrix(Matrix { values: elements, rows: num_rows, columns: num_columns })
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Array::Scalar(scalar) => write!(f, "{}", scalar),
            Array::Linear(linear) => {
                write!(f, "[")?;
                for (i, value) in linear.values.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if (i as u16) < linear.len - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            Array::Matrix(matrix) => {
                let column_widths: Vec<usize> = (0..matrix.columns).map(|c| matrix.values
                    .iter()
                    .map(|row| row.get(c as usize).unwrap().to_string().len())
                    .max()
                    .unwrap_or_default())
                    .collect();

                write!(f, "[")?;
                for (y, row) in matrix.values.iter().enumerate() {
                    if y > 0 {
                        write!(f, " ")?;
                    }

                    for (x, col) in row.iter().enumerate() {
                        let column_width = column_widths[x];
                        write!(f, "{:>column_width$}", col)?;
                        if (x as u16) < matrix.columns - 1 {
                            write!(f, " ")?;
                        }
                    }

                    if (y as u16) < matrix.rows - 1 {
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
    use crate::location::Span;
    use super::*;
    
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