use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Shape {
    Scalar,
    Linear(u16),
    Matrix(u16, u16),
}

#[derive(Clone, Debug)]
pub struct Array<T> {
    pub shape: Shape,
    pub values: Vec<T>,
}

impl<T> Array<T> {
    pub fn new(shape: Shape, values: Vec<T>) -> Self {
        Array { shape, values }
    }

    pub fn scalar(element: T) -> Self {
        Self::new(Shape::Scalar, vec![element])
    }

    pub fn linear(elements: Vec<T>) -> Self {
        Self::new(Shape::Linear(elements.len() as u16), elements)
    }

    pub fn matrix(elements: Vec<Vec<T>>) -> Self {
        let num_rows = elements.len() as u16;
        let num_columns = elements.get(0).map(|x| x.len()).unwrap_or(0) as u16;
        let elements = elements.into_iter().flatten().collect();
        Self::new(Shape::Matrix(num_columns, num_rows), elements)
    }
}

impl<T> Display for Array<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.shape {
            Shape::Scalar => write!(f, "{}", self.values.first().unwrap()),
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
                        let column_width = column_widths[column as usize];
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

        let multiple_rows_one_column = Array::matrix(
            vec![
                vec![2],
                vec![3],
                vec![4]]);
        assert_eq!(multiple_rows_one_column.to_string(),
                   concat!("[2\n",
                   " 3\n",
                   " 4]"));

        let multiples_rows_multiple_columns = Array::matrix(
            vec![
                vec![1, 2, 3],
                vec![4, 5, 6],
                vec![7, 8, 9]]);
        assert_eq!(multiples_rows_multiple_columns.to_string(),
                   concat!("[1 2 3\n",
                   " 4 5 6\n",
                   " 7 8 9]"));

        let column_values_are_padded = Array::matrix(
            vec![
                vec![1, 222, 3],
                vec![44, 55, 6],
                vec![7, 8, 90]]);
        assert_eq!(column_values_are_padded.to_string(),
                   concat!("[ 1 222  3\n",
                   " 44  55  6\n",
                   "  7   8 90]"));
    }
}