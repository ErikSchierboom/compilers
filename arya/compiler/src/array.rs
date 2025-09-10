use std::fmt::{Display, Formatter};

pub enum Array<T> {
    Scalar(Scalar<T>),
    Linear(Linear<T>),
    Matrix(Matrix<T>),
}

pub struct Scalar<T> {
    pub value: T,
}

pub struct Linear<T> {
    pub values: Vec<T>,
    pub len: u16,
}

pub struct Matrix<T> {
    pub values: Vec<Vec<T>>,
    pub rows: u16,
    pub columns: u16,
}

impl<T> Array<T> {
    pub fn scalar(element: T) -> Self {
        Array::Scalar(Scalar { value: element })
    }

    pub fn linear(elements: Vec<T>) -> Self {
        let length = elements.len() as u16;
        Array::Linear(Linear { values: elements, len: length })
    }

    pub fn matrix(elements: Vec<Vec<T>>) -> Self {
        let num_rows = elements.len() as u16;
        let num_columns = elements.get(0).map(|x| x.len()).unwrap_or(0) as u16;
        Array::Matrix(Matrix { values: elements, rows: num_rows, columns: num_columns })
    }
}

impl<T: Display> Display for Array<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Array::Scalar(scalar) => write!(f, "{}", scalar.value),
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
    use super::*;

    #[test]
    fn test_display_scalar() {
        let scalar = Array::scalar(5);
        assert_eq!(scalar.to_string(), "5");
    }

    #[test]
    fn test_display_linear() {
        let empty: Array<i32> = Array::linear(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single = Array::linear(vec![13]);
        assert_eq!(single.to_string(), "[13]");

        let multiples = Array::linear(vec![27, 9, 1]);
        assert_eq!(multiples.to_string(), "[27 9 1]");
    }

    #[test]
    fn test_display_matrix() {
        let empty: Array<i32> = Array::matrix(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single_row_no_columns: Array<i32> = Array::matrix(vec![vec![]]);
        assert_eq!(single_row_no_columns.to_string(), "[]");

        let single_row_one_column = Array::matrix(vec![vec![2]]);
        assert_eq!(single_row_one_column.to_string(), "[2]");

        let multiple_rows_one_column = Array::matrix(vec![vec![2], vec![3], vec![4]]);
        assert_eq!(multiple_rows_one_column.to_string(), 
                   concat!("[2\n",
                           " 3\n",
                           " 4]"));

        let multiples_rows_multiple_columns = Array::matrix(vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]]);
        assert_eq!(multiples_rows_multiple_columns.to_string(), 
                   concat!("[1 2 3\n",
                           " 4 5 6\n",
                           " 7 8 9]"));

        let column_values_are_padded = Array::matrix(vec![vec![1, 222, 3], vec![44, 55, 6], vec![7, 8, 90]]);
        assert_eq!(column_values_are_padded.to_string(),
                   concat!("[ 1 222  3\n",
                           " 44  55  6\n",
                           "  7   8 90]"));
    }
}