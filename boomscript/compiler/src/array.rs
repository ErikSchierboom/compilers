use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Shape {
    pub dimensions: Vec<usize>,
}

impl Shape {
    pub fn new(dimensions: Vec<usize>) -> Self {
        Self { dimensions }
    }

    pub fn empty() -> Self {
        Self::new(vec![])
    }

    pub fn scalar() -> Self {
        Self { dimensions: Vec::new() }
    }

    pub fn row_count(&self) -> usize {
        self.dimensions.first().copied().unwrap_or(0)
    }

    pub fn row_len(&self) -> usize {
        self.dimensions.iter().skip(1).product()
    }

    pub fn is_scalar(&self) -> bool {
        self.dimensions.is_empty()
    }

    pub fn is_one_dimensional(&self) -> bool {
        self.dimensions.len() == 1
    }

    pub fn prepend_dimension(&mut self, dimension: usize) {
        self.dimensions.insert(0, dimension)
    }

    pub fn replace_dimension(&mut self, index: usize, dimension: usize) {
        self.dimensions[index] = dimension
    }
}

#[derive(Clone, Debug)]
pub struct Array<T> {
    pub shape: Shape,
    pub elements: Vec<T>,
}

impl<T> Array<T> {
    pub fn new(shape: Shape, values: Vec<T>) -> Self {
        Self { shape, elements: values }
    }

    pub fn scalar(element: T) -> Self {
        Self::new(Shape::scalar(), vec![element])
    }

    pub fn empty() -> Self {
        Self::new(Shape::new(vec![0]), Vec::new())
    }

    pub fn linear(elements: Vec<T>) -> Self {
        Self::new(Shape::new(vec![elements.len()]), elements)
    }

    pub fn matrix(elements: Vec<Vec<T>>) -> Self {
        Self::new(Shape::new(vec![elements.len(), elements.first().map(|row| row.len()).unwrap_or(0)]), elements.into_iter().flatten().collect())
    }

    pub fn row_slices(&self) -> impl ExactSizeIterator<Item=&[T]> {
        self.elements.chunks_exact(self.shape.row_len())
    }

    pub fn monadic_op<V>(top: Self, bottom: Self, op: impl Fn(&T) -> V) -> Array<V> {
        let new_values: Vec<V> = bottom.elements.into_iter().map(|a| op(&a)).collect();
        Array::new(top.shape.clone(), new_values)
    }

    pub fn monadic_op_mut(mut array: Self, op: impl Fn(&T) -> T) -> Self {
        for a in array.elements.iter_mut() {
            *a = op(&a)
        }

        array
    }

    pub fn dyadic_op_mut(mut top: Self, mut bottom: Self, op: impl Fn(&T, &T) -> T) -> Result<Self, ()> {
        if top.shape == bottom.shape {
            for (b, a) in bottom.elements.iter_mut().zip(top.elements) {
                *b = op(&a, b)
            }
            Ok(bottom)
        } else if top.shape.is_scalar() {
            let a = top.elements.pop().unwrap();

            for b in bottom.elements.iter_mut() {
                *b = op(&a, b)
            }

            Ok(bottom)
        } else if bottom.shape.is_scalar() {
            let a = bottom.elements.pop().unwrap();

            for b in top.elements.iter_mut() {
                *b = op(&a, b)
            }

            Ok(top)
        } else {
            todo!("error handling")
        }
    }
}

impl<T> Display for Array<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.shape.dimensions.len() {
            0 => write!(f, "[]"),
            1 => {
                write!(f, "[")?;
                for (column, value) in self.elements.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if column < self.shape.dimensions[0] - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
            2 => {
                let row_len = self.shape.row_len();
                let row_count = self.shape.row_count();

                let column_widths: Vec<usize> = (0..row_len).map(|column|
                    (0..row_count).map(|row|
                        self.elements.get(row * row_len + column).unwrap().to_string().len()
                    ).max().unwrap_or_default())
                    .collect();

                write!(f, "[")?;

                for row in 0..row_count {
                    if row > 0 {
                        write!(f, " ")?;
                    }

                    for column in 0..row_len {
                        let column_width = column_widths[column];
                        let value = self.elements.get(row * row_len + column).unwrap();
                        write!(f, "{:>column_width$}", value)?;
                        if column < row_len - 1 {
                            write!(f, " ")?;
                        }
                    }

                    if row < row_count - 1 {
                        write!(f, "\n")?;
                    }
                }

                write!(f, "]")
            }
            n => write!(f, "{n}-dimensional array")
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
        let empty: Array<i64> = Array::linear(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single = Array::linear(vec![13]);
        assert_eq!(single.to_string(), "[13]");

        let multiples = Array::linear(vec![27, 9, 1]);
        assert_eq!(multiples.to_string(), "[27 9 1]");
    }

    #[test]
    fn test_display_matrix() {
        let empty: Array<i64> = Array::matrix(vec![]);
        assert_eq!(empty.to_string(), "[]");

        let single_row_no_columns: Array<i64> = Array::matrix(vec![vec![]]);
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